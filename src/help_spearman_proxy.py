from typing import Optional, Literal, Union

import numpy as np
import pandas as pd
from sklearn.model_selection import KFold
import torch
import xgboost as xgb

# we use a local copy of the fast_soft_sort manually downloaded from
# https://github.com/google-research/fast-soft-sort/tree/master
from fast_soft_sort.pytorch_ops import soft_rank

REG_STRENGTH = 1.1#0.090
SOFT = True

def init_model(n_features: int, lr: float, weight_decay: float) -> tuple[object, object]:
    """Initialize model.
        Params:
            n_features: number of features the model will have
            lr: learning rate
            weight_decay: L2 regularization strength
        Return:
            initialized model and optimizer
    """
    torch.manual_seed(2023)
    model = torch.nn.Linear(n_features, 1, bias=True)
    optimizer = torch.optim.SGD(model.parameters(), lr=lr, weight_decay=weight_decay)

    return model, optimizer

def train_step(
        X: torch.Tensor,
        y: torch.Tensor,
        model: object,
        loss_fn: object,
        optimizer: object
        ) -> torch.Tensor:
    """Make one training step."""
    # Compute prediction and loss
    y_hat = model(X)
    loss = loss_fn(y, y_hat)

    # Backpropagation
    loss.backward()
    optimizer.step()
    optimizer.zero_grad() # did not forget!
    # print(model.weight.grad, model.bias.grad)

    return loss

def get_model_params(model):
    """Extract model params"""
    return torch.cat([param.data.squeeze().reshape(1, -1) for param in model.parameters()], dim=1)

def train(
        X_train: torch.Tensor,
        y_train: torch.Tensor,
        X_test: torch.Tensor,
        y_test: torch.Tensor,
        n_steps: int,
        lr: float,
        weight_decay: float,
        loss_fn: object,
        eval_fns: dict[str, object]
        ) -> tuple[object, pd.DataFrame]:
    """Train LR model."""
    model, optimizer = init_model(X_train.size()[1], lr=lr, weight_decay=weight_decay)

    # Init 'stats' dict that will store training and eval stats and model params
    # evolution. 1st train loss before 1st step will be added in loop.
    stats = {"training loss": [], "test loss": [loss_fn(y_test, model(X_test)).item()]}
    for eval_name, eval_fn in eval_fns.items():
        stats[f"training {eval_name}"] = [eval_fn(y_train, model(X_train)).item()]
        stats[f"test {eval_name}"] = [eval_fn(y_test, model(X_test)).item()]
    
    stats_params = [get_model_params(model)]
    
    # make training steps
    for epoch in range(n_steps):
        train_loss = train_step(X_train, y_train, model, loss_fn, optimizer)
        test_loss = loss_fn(y_test, model(X_test))
        stats["training loss"].append(train_loss.item())
        stats["test loss"].append(test_loss.item())
        for eval_name, eval_fn in eval_fns.items():
            stats[f"training {eval_name}"].append(eval_fn(y_train, model(X_train)).item())
            stats[f"test {eval_name}"].append(eval_fn(y_test, model(X_test)).item())
        stats_params.append(get_model_params(model))

    stats["training loss"].append(loss_fn(y_train, model(X_train)).item())
    
    print("Final model params:")
    for name, param in model.named_parameters():
        print(f"{name}:\t{param}")
    
    return model, pd.DataFrame(stats), pd.DataFrame(torch.cat(stats_params).detach().numpy())

def train_cv(
        X_train: torch.Tensor,
        y_train: torch.Tensor,
        n_steps: int,
        lr: float,
        weight_decay: float,
        loss_fn: object,
        precision: float = 1e-6
        ) -> tuple[object, float]:
    """Train LR model in a cross-validation loop."""
    model, optimizer = init_model(X_train.size()[1], lr=lr, weight_decay=weight_decay)
    
    # make training steps
    for step in range(1, n_steps + 1):
        loss_old = train_step(X_train, y_train, model, loss_fn, optimizer)
        loss_new = loss_fn(y_train, model(X_train))
        if loss_old - loss_new < precision: break

    return model, loss_new

def cv_loop(
        X: torch.Tensor,
        y: torch.Tensor,
        loss_fn: object,
        lr: float,
        weight_decay: float,
        eval_fn: Optional[dict[str, object]] = {},
        n_splits: int = 5,
        precision: float = 1e-6
        ) -> None:
    """Cross-validation loop.

    Args:
        X (torch.Tensor): Training data
        y (torch.Tensor): Target
        loss_fn (object): loss function
        lr (float): learning rate
        weight_decay (float): L2 regularization strength
        eval_fn (Optional[dict[str, object]], optional): evaluation
            functions. Defaults to {}.
        n_splits (int, optional): Number of cross-validation splits.
            Defaults to 5. Defaults to 5.
        precision (float, optional): Precision of the gradient descent.
            Defaults to 1e-6.

    Returns:
        _type_: _description_
    """
    stats_loss = {"train": [], "val": []}
    stats_eval = {}
    for name in eval_fn:
        stats_eval["train " + name] = []
        stats_eval["val " + name] = []

    for train_idx, val_idx in KFold(n_splits).split(X):
        X_train = X[train_idx, :]
        y_train = y[train_idx, :]
        X_val = X[val_idx, :]
        y_val = y[val_idx, :]

        model, loss_train = \
            train_cv(
                X_train,
                y_train,
                n_steps=3000,
                lr=lr,
                weight_decay=weight_decay,
                loss_fn=loss_fn,
                precision=precision
                )
        
        loss_val = loss_fn(y_val, model(X_val))

        # eval fns
        for name, f in eval_fn.items():
            stats_eval["train " + name].append(f(y_train, model(X_train)).item())
            stats_eval["val " + name].append(f(y_val, model(X_val)).item())

        stats_loss["train"].append(loss_train.item())
        stats_loss["val"].append(loss_val.item())

    stats_loss = pd.DataFrame(stats_loss)
    stats_eval = pd.DataFrame(stats_eval)

    mean_loss = stats_loss.mean()
    std_loss = stats_loss.std()
    mean_eval = stats_eval.mean()
    std_eval = stats_eval.std()
    
    return mean_loss, std_loss, mean_eval, std_eval

def rank(y: torch.Tensor) -> torch.Tensor:
    "Rank values of a vector of real numbers."
    r = torch.empty_like(y)
    r[y.argsort(dim=0), 0] = torch.arange(1, y.numel() + 1, dtype=y.dtype).reshape(-1, 1)
    return r

def normalize_rank(r: torch.Tensor) -> torch.Tensor:
    """Normalize ranks to [0, 1]."""
    # r_norm = (r - r.min()) / (r.max() - r.min()) # this normalization isn't differentiable
    r_norm = torch.nn.functional.normalize(r, dim=0)
    return r_norm

############################### Loss funcs ###############################
def loss_mse(y: torch.Tensor, y_hat: torch.Tensor) -> torch.Tensor:
    """MSE loss."""
    criterion = torch.nn.MSELoss()
    return criterion(y_hat, y)

def loss_msrd(y: torch.Tensor, y_hat: torch.Tensor, soft=SOFT) -> torch.Tensor:
    """Mean squared rank difference loss.

    Args:
        y (torch.Tensor): True target values
        y_hat (torch.Tensor): Predicted target values
        soft (_type_, optional): Evaluate soft ranks. Otherwise hard ranks will
            be estimated. Defaults to SOFT.

    Returns:
        torch.Tensor: Loss value, a 0D tensor.
    """
    # rank true values
    if soft:
        r = soft_rank(y.reshape(1, -1), regularization_strength=REG_STRENGTH).reshape(-1, 1)
    else:
        r = rank(y)
       
    # soft rank predictions
    r_hat = soft_rank(y_hat.reshape(1, -1), regularization_strength=REG_STRENGTH).reshape(-1, 1)
    
    # normalize ranks
    r = normalize_rank(r)
    r_hat = normalize_rank(r_hat)
    
    # squared rank difference (similar to the approach in "Fast Differentiable Sorting and
    # Ranking" article)
    criterion = torch.nn.MSELoss()
    loss = criterion(r, r_hat)

    return loss

def spearman(y: torch.Tensor, y_hat: torch.Tensor, soft=SOFT) -> torch.Tensor:
    # rank true values
    if soft:
        r = soft_rank(y.reshape(1, -1), regularization_strength=REG_STRENGTH).reshape(-1, 1)
    else:
        r = rank(y)
       
    # soft rank predictions
    r_hat = soft_rank(y_hat.reshape(1, -1), regularization_strength=REG_STRENGTH).reshape(-1, 1)
    
    # normalize ranks
    r = normalize_rank(r)
    r_hat = normalize_rank(r_hat)
    
    # Spearman corr
    # thiss loss might not be correctly differentiable
    # loss = 1 - torch.corrcoef(torch.cat((r.reshape(1, -1), r_hat.reshape(1, -1)), dim=0))[0, 1]
    # another version
    # cos = torch.nn.CosineSimilarity(dim=0, eps=1e-6)
    # loss = 1 - cos(r - r.mean(dim=0), r_hat - r_hat.mean(dim=0))
    r = r - r.mean(dim=0)
    r_hat = r_hat - r_hat.mean(dim=0)

    corr = (r * r_hat).sum() / (r.norm() * r_hat.norm())

    return corr

def loss_spearman(y: torch.Tensor, y_hat: torch.Tensor, soft=SOFT) -> torch.Tensor:
    """Spearman correlation loss.

    Args:
        y (torch.Tensor): True target values
        y_hat (torch.Tensor): Predicted target values
        soft (_type_, optional): Evaluate soft ranks. Otherwise hard ranks will
            be estimated. Defaults to SOFT.

    Returns:
        torch.Tensor: Loss value, a 0D tensor.
    """
    loss = 1 - spearman(y, y_hat, SOFT)

    return loss

############################### Objective funcs ###############################
def obj_spearman(
        ytrue: Union[np.ndarray, xgb.DMatrix],
        ypred: Union[np.ndarray, xgb.DMatrix]
        ) -> tuple[np.ndarray, np.ndarray]:
    """Compute gradient and hessian of the Spearman corr as objective func.

    Args:
        ytrue (Union[np.ndarray, xgb.DMatrix]): _description_
        ypred (Union[np.ndarray, xgb.DMatrix]): _description_

    Returns:
        tuple[np.ndarray, np.ndarray]: _description_
    """
    # convert DMatrix to ndarray, for the use in xgb.cv()
    if isinstance(ytrue, xgb.core.DMatrix):
        ytrue = ytrue.get_label()
    if isinstance(ypred, xgb.core.DMatrix):
        ypred = ypred.get_label()
    
    # soft rank expects a 2d tensor
    ytrue = ytrue.reshape(1, -1)
    ypred = ypred.reshape(1, -1)

    # convert to pytorch tensors
    ytrue_th = torch.tensor(ytrue)
    ypred_th = torch.tensor(ypred, requires_grad=True)

    spear_th = 1 - spearman(ytrue_th, ypred_th)
    grad_th = torch.autograd.grad(spear_th, ypred_th)

    # Convert grad to numpy, set hessian to ones since soft rank is not twice
    # differantiable (seems that internally at some point soft_rank uses numpy
    # arrays which detaches the operations from the computational graph of pytorch).
    grad = grad_th[0].detach().numpy().squeeze()
    hess = np.ones(grad.shape)
    
    return grad, hess

############################### Metrics ###############################
def eval_mse(y: torch.Tensor, y_hat: torch.Tensor) -> torch.Tensor:
    """MSE eval."""
    criterion = torch.nn.MSELoss()
    return criterion(y_hat, y)

def eval_hard_msrd(y: torch.Tensor, y_hat: torch.Tensor) -> torch.Tensor:
    """Mean squared hard rank difference metric."""
    # exact ranks
    r = rank(y)
    r_hat = rank(y_hat)
    
    # normalize ranks
    r = normalize_rank(r)
    r_hat = normalize_rank(r_hat)
    
    # squared rank difference (similar to the approach in "Fast Differentiable Sorting and
    # Ranking" article)
    criterion = torch.nn.MSELoss()
    eval = criterion(r, r_hat)

    return eval

def eval_soft_msrd(y: torch.Tensor, y_hat: torch.Tensor) -> torch.Tensor:
    """Mean squared soft rank difference metric."""
    # soft ranks
    r = soft_rank(y.reshape(1, -1)).reshape(-1, 1)
    r_hat = soft_rank(y_hat.reshape(1, -1)).reshape(-1, 1)
    
    # normalize ranks
    r = normalize_rank(r)
    r_hat = normalize_rank(r_hat)
    
    # squared rank difference (similar to the approach in "Fast Differentiable Sorting and
    # Ranking" article)
    criterion = torch.nn.MSELoss()
    eval = criterion(r, r_hat)

    return eval

def eval_hard_spearman(y: torch.Tensor, y_hat: torch.Tensor) -> torch.Tensor:
    """(hard) Spearman corr metric"""
    y = y.reshape(1, -1).squeeze().numpy()
    y_hat = y_hat.reshape(1, -1).squeeze().detach().numpy()
    eval = pd.DataFrame({"y": y, "y_hat": y_hat}).corr("spearman").iloc[0, 1]
    return torch.tensor(eval)

def eval_soft_spearman(y: torch.Tensor, y_hat: torch.Tensor) -> torch.Tensor:
    """(soft) Spearman corr metric"""
    # soft ranks
    r = soft_rank(y.reshape(1, -1)).reshape(-1, 1)
    r_hat = soft_rank(y_hat.reshape(1, -1)).reshape(-1, 1)
    
    # normalize ranks
    r = normalize_rank(r)
    r_hat = normalize_rank(r_hat)
    
    # Spearman corr
    eval = torch.corrcoef(torch.cat((r.reshape(1, -1), r_hat.reshape(1, -1)), dim=0))[0, 1]
    
    return eval
