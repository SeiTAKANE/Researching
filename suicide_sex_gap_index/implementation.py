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
# Japan: compute w across all periods
# =========================
_df_japan_base = (
    df_std_wide[
        (df_std_wide["location"] == "Japan")
        & (df_std_wide["period"].between(2000, 2021))
    ]
    .copy()
)

t0_japan = _df_japan_base[_df_japan_base["period"].between(2000, 2006)]["Overall"].mean()
proj = Projection(T0=t0_japan)

df_japan = _df_japan_base.copy()
df_japan["w"] = proj.projection_w(df_japan["D"].values, df_japan["Overall"].values)


# =========================
# trend of w in Japan
# =========================
# p_japan_w = (
#     ggplot(df_japan, aes(x="period", y="w"))
#     + geom_line(size=1.1)
#     + geom_point(size=2.2)
#     + scale_x_continuous(breaks=range(
#         df_japan["period"].min(),
#         df_japan["period"].max() + 1,
#         2
#     ))
#     + labs(
#         title="Japan: w trend",
#         x="Year",
#         y="w",
#     )
#     + theme_bw()
#     + theme(
#         plot_title=element_text(size=14, face="bold"),
#         axis_title=element_text(size=11),
#         axis_text=element_text(size=9),
#     )
# )

# p_japan_w.save("results/p_japan_w_trend.png", dpi=350, width=19, height=12, units="cm")

# =========================
# index trends (2000-2006 avg = 100): D, R, T, w
# =========================
_base_avg = df_japan[df_japan["period"].between(2000, 2006)][["D", "Overall", "R", "w"]].mean()

_line_cats = ["D", "R", "T", "w"]

df_japan_idx_long = (
    df_japan
    .assign(
        D=lambda df: df["D"] / _base_avg["D"] * 100,
        R=lambda df: df["R"] / _base_avg["R"] * 100,
        T=lambda df: df["Overall"] / _base_avg["Overall"] * 100,
        w=lambda df: df["w"] / _base_avg["w"] * 100,
    )
    .melt(id_vars="period", value_vars=["D", "R", "T", "w"], var_name="line_label", value_name="index_val")
    [["period", "line_label", "index_val"]]
    .assign(line_label=lambda df: pd.Categorical(df["line_label"], categories=_line_cats, ordered=True))
)

p_japan_idx_trend = (
    ggplot(df_japan_idx_long, aes(x="period", y="index_val", color="line_label", group="line_label"))
    + geom_hline(yintercept=100, linetype="dashed", color="black", size=0.6, alpha=0.5)
    + geom_line(size=1.0, alpha=0.9)
    + geom_point(size=2.0, alpha=0.9)
    + scale_x_continuous(breaks=range(
        df_japan["period"].min(),
        df_japan["period"].max() + 1,
        2
    ))
    + scale_color_brewer(type="qual", palette="Dark2", name="Indicator")
    + labs(
        title="Trends in indexed values for each indicator in Japan (2000–2006 avg = 100)",
        x="Year",
        y="Index (2000–2006 avg = 100)",
    )
    + theme_bw()
    + theme(
        plot_title=element_text(size=14, face="bold"),
        axis_title=element_text(size=11),
        axis_text=element_text(size=9),
        legend_position="bottom",
    )
)

p_japan_idx_trend.save("results/p_japan_idx_trend.png", dpi=350, width=19, height=12, units="cm")


# =========================
# 3-period comparison table: D, R, T, w (Japan)
# =========================
_period_bins = [1999, 2006, 2013, 2021]
_period_labels = ["2000-2006", "2007-2013", "2014-2021"]

df_japan_table = (
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
    .groupby("period_group", observed=True)[["D", "R", "Overall", "w"]]
    .mean()
    .round(2)
    .rename(columns={"Overall": "T"})
)

_base_row = df_japan_table.loc["2000-2006"]

def _fmt_with_idx(val, base):
    idx = round(val / base * 100, 2)
    return f"{val:.2f} ({idx:.2f})"

df_japan_table_fmt = df_japan_table.copy().astype(str)
for period in ["2007-2013", "2014-2021"]:
    for col in df_japan_table.columns:
        df_japan_table_fmt.loc[period, col] = _fmt_with_idx(
            df_japan_table.loc[period, col],
            _base_row[col],
        )

df_japan_table_fmt.to_csv("results/japan_period_comparison.csv")

