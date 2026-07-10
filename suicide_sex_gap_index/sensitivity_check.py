import numpy as np
import pandas as pd
from utils.projections import Projection
from plotnine import *


# =========================
# data
# =========================
df_who_std = pd.read_csv("data/who_data_std.csv")

df_std_wide = (
    df_who_std[
        [
            "Period",
            "ParentLocation",
            "Location",
            "Dim1",
            "FactValueNumeric",
        ]
    ]
    .assign(
        Dim1=lambda df: pd.Categorical(df["Dim1"]),
    )
    .rename(
        columns={
            "Period": "period",
            "ParentLocation": "parentLocation",
            "Location": "location",
            "FactValueNumeric": "std_rate",
            "Dim1": "sex",
        }
    )
    .pipe(lambda df: df[df["sex"].isin(["Male", "Female", "Both sexes"])])
    .dropna(subset=["std_rate"])
    .pivot_table(
        index=["period", "parentLocation", "location"],
        columns="sex",
        values="std_rate",
        aggfunc="first",
    )
    .reset_index()
    .rename(columns={"Both sexes": "Overall"})
    .assign(
        R=lambda df: df["Male"] / df["Female"],
        D=lambda df: df["Male"] - df["Female"],
    )
)


# =========================
# Japan: compute w under alternative T0 definitions
# =========================
_df_japan_base = (
    df_std_wide[
        (df_std_wide["location"] == "Japan")
        & (df_std_wide["period"].between(2000, 2021))
    ]
    .copy()
)

_t0_defs = {
    "2000-2006 avg": _df_japan_base[_df_japan_base["period"].between(2000, 2006)]["Overall"].mean(),
}
for _year in range(2000, 2007):
    _t0_defs[f"{_year} only"] = _df_japan_base.loc[_df_japan_base["period"] == _year, "Overall"].iloc[0]
_t0_cats = list(_t0_defs.keys())

df_japan = _df_japan_base.copy()
for label, t0_val in _t0_defs.items():
    proj = Projection(T0=t0_val)
    df_japan[label] = proj.projection_w(df_japan["D"].values, df_japan["Overall"].values)


# =========================
# 3-period comparison table: w under each T0 definition (Japan)
# =========================
_period_bins = [1999, 2006, 2013, 2021]
_period_labels = ["2000-2006", "2007-2013", "2014-2021"]

df_japan_w_table = (
    df_japan
    .assign(
        period_group=lambda df: pd.cut(
            df["period"],
            bins=_period_bins,
            labels=_period_labels,
            right=True,
        )
    )
    .dropna(subset=["period_group"])
    .groupby("period_group", observed=True)[_t0_cats]
    .mean()
    .round(2)
)

_base_row_w = df_japan_w_table.loc["2000-2006"]

def _fmt_with_idx(val, base):
    idx = round(val / base * 100, 2)
    return f"{val:.2f} ({idx:.2f})"

df_japan_w_table_fmt = df_japan_w_table.copy().astype(str)
for period in ["2007-2013", "2014-2021"]:
    for col in df_japan_w_table.columns:
        df_japan_w_table_fmt.loc[period, col] = _fmt_with_idx(
            df_japan_w_table.loc[period, col],
            _base_row_w[col],
        )

df_japan_w_table_fmt.to_csv("results/japan_w_T0_sensitivity_table.csv")
