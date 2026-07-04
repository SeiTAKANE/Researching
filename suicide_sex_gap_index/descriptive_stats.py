import numpy as np
import pandas as pd


# Map parentLocation to short code
parent_location_map = {
    "Americas": "AMR",
    "Eastern Mediterranean": "EMR",
    "South-East Asia": "SEAR",
    "Africa": "AFR",
    "Europe": "EUR",
    "Western Pacific": "WPR",
}


# Read WHO standardized data
df_who_std = pd.read_csv("data/who_data_std.csv")

# Select and rename columns, set dtypes
df_filtered_who_std = (
    df_who_std[
        [
            "Period",
            "ParentLocation",
            "Location",
            "Dim1",
            "FactValueNumeric",
            "FactValueNumericLow",
            "FactValueNumericHigh",
        ]
    ]
    .assign(
        ParentLocation=lambda df: pd.Categorical(df["ParentLocation"]),
        Location=lambda df: pd.Categorical(df["Location"]),
        Dim1=lambda df: pd.Categorical(df["Dim1"]),
    )
    .rename(
        columns={
            "Period": "period",
            "ParentLocation": "parentLocation",
            "Location": "location",
            "FactValueNumeric": "std_rate",
            "FactValueNumericLow": "std_rate_low",
            "FactValueNumericHigh": "std_rate_high",
            "Dim1": "sex",
        }
    )
)



df_filtered_who_std = df_filtered_who_std.assign(
    parentLocation_code=df_filtered_who_std["parentLocation"].astype(str).map(
        lambda x: parent_location_map.get(x, x)
    )
)

# Male/Female ratio for standardized data
df_std_wide = (
    df_filtered_who_std[
        df_filtered_who_std["sex"].isin(["Male", "Female"])
    ][["period", "parentLocation", "parentLocation_code", "location", "sex", "std_rate"]]
    .dropna(subset=["std_rate"])
    .pivot_table(
        index=["period", "parentLocation", "parentLocation_code", "location"],
        columns="sex",
        values="std_rate",
        aggfunc="first",
    )
    .reset_index()
)



# Pivot creates columns "Male" / "Female"; rename to std_rate_Male / std_rate_Female
df_std_wide = df_std_wide.rename(columns={"Male": "std_rate_Male", "Female": "std_rate_Female"})

mf_ratio_std = df_std_wide.assign(
    mf_ratio=df_std_wide["std_rate_Male"] / df_std_wide["std_rate_Female"]
).loc[
    lambda x: np.isfinite(x["mf_ratio"]) & (x["std_rate_Female"] > 0)
]

# Create table directly from mf_ratio (avoiding average of averages)
mf_ratio_table = (
    mf_ratio_std.loc[(mf_ratio_std["period"] >= 2000) & (mf_ratio_std["period"] <= 2021)]
    .assign(
        period_range=lambda df: pd.cut(
            df["period"],
            bins=[2000, 2005, 2010, 2015, 2022],
            labels=["2000-2004", "2005-2009", "2010-2014", "2015-2021"],
            right=False,
        ).astype(str)
    )
    .groupby(["parentLocation", "period_range"], as_index=False)["mf_ratio"]
    .agg(mean_mf_ratio=lambda x: round(x.mean(), 2))
)
# World summary (mean over all regions for each period range)
world_summary = (
    mf_ratio_std.loc[(mf_ratio_std["period"] >= 2000) & (mf_ratio_std["period"] <= 2021)]
    .assign(
        period_range=lambda df: pd.cut(
            df["period"],
            bins=[2000, 2005, 2010, 2015, 2022],
            labels=["2000-2004", "2005-2009", "2010-2014", "2015-2021"],
            right=False,
        ).astype(str)
    )
    .groupby("period_range", as_index=False)["mf_ratio"]
    .agg(mean_mf_ratio=lambda x: round(x.mean(), 2))
    .assign(parentLocation="World")
)
mf_ratio_table = (
    pd.concat([mf_ratio_table, world_summary], ignore_index=True)
    .pivot_table(
        index="parentLocation",
        columns="period_range",
        values="mean_mf_ratio",
        aggfunc="first",
    )
    .reset_index()
)

# World first, then others alphabetically
mf_ratio_table = mf_ratio_table.sort_values(
    "parentLocation", key=lambda s: (s != "World").astype(int).astype(str) + s
).rename(columns={"parentLocation": "Location"})


# Save world mf ratio table to csv
mf_ratio_table.to_csv("results/world_mf_ratio_table.csv", index=False)

# # Create location master table for mapping parentLocation and location to codes
# df_location_master = (
#     df_filtered_who_std[["parentLocation", "location"]]
#     .drop_duplicates()
#     .sort_values(["parentLocation", "location"])
#     .reset_index(drop=True)
# )

# df_location_master.to_csv("results/location_master.csv", index=False)
