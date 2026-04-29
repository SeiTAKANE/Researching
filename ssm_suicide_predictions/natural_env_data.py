import geopandas as gpd
import numpy as np
import pandas as pd
import requests
from time import sleep
import ast
import datetime
import requests
from collections import defaultdict
from shapely.geometry import Point, box
from shapely.prepared import prep
import os
import math
import time
# URL (or local path) of the GeoJSON file
url = "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_2021_4326_LEVL_2.geojson"

# Load GeoJSON (read directly from URL)
gdf = gpd.read_file(url)
# Extract only Spain (CNTR_CODE == 'ES')
spain = gdf[gdf["CNTR_CODE"] == "ES"].copy()

# Add bounding box
spain["min_lon"] = spain.bounds.minx
spain["max_lon"] = spain.bounds.maxx
spain["min_lat"] = spain.bounds.miny
spain["max_lat"] = spain.bounds.maxy

# lats, lons column to list format
spain["lats"] = spain[["min_lat", "max_lat"]].values.tolist()
spain["lons"] = spain[["min_lon", "max_lon"]].values.tolist()

# dictionary of region name to polygon
region2geom = dict(zip(spain['NUTS_NAME'], spain['geometry']))
# select and rename columns
df_geo = spain[["NUTS_NAME", "lats", "lons"]].rename(columns={"NUTS_NAME": "name"})

# Set save directory to relative path
save_dir = "data/geo_base"
os.makedirs(save_dir, exist_ok=True)

# Region name → Polygon (both raw and prepared versions)
region2geom_raw  = dict(zip(spain['NUTS_NAME'], spain['geometry']))
region2geom_prep = {n: prep(g) for n, g in region2geom_raw.items()}

# Time period and grid resolution (match original resolution)
START_Y, END_Y = 2003, 2023
LAT_STEP_T2M, LON_STEP_T2M = 0.5, 0.625   # Temperature (MERRA-2 grid)
LAT_STEP_SWR, LON_STEP_SWR = 1.0, 1.0     # Solar radiation (CERES 1deg)
EPS = 1e-12

# BASE URL
BASE = "https://power.larc.nasa.gov/api/temporal/monthly/point"
COMMON_QS = f"?start={START_Y}&end={END_Y}&community=AG&format=JSON"

session = requests.Session()
cache = {}  # {(param, lat4, lon4): {YYYYMM: val}}

def backoff_sleep(k):
    time.sleep(min(0.2 * (2 ** k), 6.4))

def fetch_point(lat, lon, param, timeout=30, max_retry=5):
    key = (param, round(lat, 4), round(lon, 4))
    if key in cache:
        return cache[key]
    url = f"{BASE}{COMMON_QS}&latitude={lat:.4f}&longitude={lon:.4f}&parameters={param}"
    last_err = None
    for k in range(max_retry):
        try:
            r = session.get(url, timeout=timeout)
            if r.status_code == 429 or r.status_code >= 500:
                last_err = Exception(f"HTTP {r.status_code}")
                backoff_sleep(k)
                continue
            r.raise_for_status()
            js = r.json()
            series = js.get("properties", {}).get("parameter", {}).get(param, {})
            cache[key] = series
            return series
        except Exception as e:
            last_err = e
            backoff_sleep(k)
    print(f"[WARN] fetch_point failed ({lat:.4f},{lon:.4f}) {param}: {last_err}")
    cache[key] = {}
    return {}

def make_axis(vmin, vmax, step):
    n = int(math.floor((vmax - vmin) / step + EPS)) + 1
    arr = np.array([vmin + i * step for i in range(n)], dtype=float)
    if arr.size == 0 or arr[-1] < vmax - EPS:
        arr = np.append(arr, vmax)
    # Handle numerical precision issues
    arr = np.unique(np.round(arr / step) * step)
    return arr

def aggregate_overlap(poly_geom, lat_min, lat_max, lon_min, lon_max,
                      lat_step, lon_step, param, label):
    """Weight by cell polygon × polygon intersection area, with cosine latitude area correction"""
    lats = make_axis(lat_min, lat_max, lat_step)
    lons = make_axis(lon_min, lon_max, lon_step)
    num, den = defaultdict(float), defaultdict(float)

    total = len(lats) * len(lons)
    done = 0
    in_cells = 0

    for lat in lats:
        cosw = math.cos(math.radians(lat))
        for lon in lons:
            done += 1
            # Cell polygon (center at (lat,lon), half-width = step/2)
            cell = box(lon - lon_step/2, lat - lat_step/2,
                       lon + lon_step/2, lat + lat_step/2)
            inter = cell.intersection(poly_geom)
            if inter.is_empty:
                if done % 50 == 0 or done == total:
                    print(f"    {label}: {done}/{total} ({done/total*100:.1f}%)")
                continue

            in_cells += 1
            w = (inter.area / cell.area) * cosw  # Intersection area ratio × cos(φ)

            series = fetch_point(lat, lon, param)
            for ym, val in series.items():
                if val is None:
                    continue
                try:
                    m = int(ym[4:])
                except Exception:
                    continue
                if 1 <= m <= 12:
                    num[ym] += w * float(val)
                    den[ym] += w

        if done % 50 == 0 or done == total:
            print(f"    {label}: {done}/{total} ({done/total*100:.1f}%)")

    print(f"    {label}: in-polygon cells = {in_cells}")
    return num, den

total_regions = len(df_geo)
for i, row in df_geo.iterrows():
    region_name = row["name"]
    safe_region_name = region_name.replace(" ", "_")
    save_path = os.path.join(save_dir, f"{safe_region_name}.csv")
    if os.path.exists(save_path):
        print(f"[{i+1}/{total_regions}] Skip {region_name} (exists)")
        continue

    lat_min, lat_max = row["lats"]
    lon_min, lon_max = row["lons"]

    poly_geom = region2geom_raw.get(region_name, None)
    if poly_geom is None:
        print(f"[{i+1}/{total_regions}] WARN: polygon not found for {region_name}, skip")
        continue

    print(f"\n=== Region [{i+1}/{total_regions}]: {region_name} ===")
    print("  Collecting T2M (0.5°×0.625°, cell-overlap weighting)...")
    t_num, t_den = aggregate_overlap(poly_geom, lat_min, lat_max, lon_min, lon_max,
                                     LAT_STEP_T2M, LON_STEP_T2M, "T2M", "T2M")

    print("  Collecting ALLSKY_SFC_SW_DWN (1°×1°, cell-overlap weighting)...")
    s_num, s_den = aggregate_overlap(poly_geom, lat_min, lat_max, lon_min, lon_max,
                                     LAT_STEP_SWR, LON_STEP_SWR, "ALLSKY_SFC_SW_DWN", "ALLSKY")

    # Only months with both temperature and solar data
    months = sorted(set(t_num.keys()) & set(s_num.keys()))
    if not months:
        print(f"  -> No valid months for {region_name}. Skipped.")
        continue

    rows = []
    for ym in months:
        y, m = int(ym[:4]), int(ym[4:])
        tden, sden = t_den.get(ym, 0.0), s_den.get(ym, 0.0)
        if tden <= 0 or sden <= 0:
            continue
        t = t_num[ym] / tden
        s = s_num[ym] / sden
        rows.append({"region": region_name, "year": y, "month": m,
                     "solar": s,  # MJ/m^2/day
                     "temp":  t}) # °C

    if not rows:
        print(f"  -> All months NaN for {region_name}. Skipped.")
        continue

    df_region = pd.DataFrame(rows).sort_values(["year", "month"])
    df_region.to_csv(save_path, index=False)
    print(f"  Saved -> {save_path}")


#create natural env data
file_path_geo_data = 'data/geo_base'
csv_files = [f for f in os.listdir(file_path_geo_data) if f.endswith('.csv')]
df_all = pd.concat(
    [pd.read_csv(os.path.join(file_path_geo_data, f)) for f in csv_files],
    ignore_index=True
)

df_all['term'] = pd.to_datetime(dict(year=df_all['year'], month=df_all['month'], day=1))

# spain area
spain_area = (
    gdf.loc[gdf['CNTR_CODE'] == 'ES', ['NUTS_ID', 'NUTS_NAME', 'geometry']]
       .reset_index(drop=True)
)

# Convert to equal-area CRS and obtain region area [km^2] (for Europe, LAEA Europe: EPSG:3035 is standard)
spain_eq = spain_area.to_crs(3035)  # equal-area CRS
spain_area['area_km2'] = spain_eq.geometry.area / 1e6  # m^2 → km^2


# Create dictionary of region name to area [km^2] (assuming df_all['region'] matches NUTS_NAME)
region2area = dict(zip(spain_area['NUTS_NAME'], spain_area['area_km2']))

# weight by area
df_all['area_w'] = df_all['region'].map(region2area)
# weight by area and aggregate by month
def wavg(group, col, wcol='area_w'):
    x = group[col].to_numpy()
    w = group[wcol].to_numpy()
    return np.average(x, weights=w)

df_grouped = (
    df_all.groupby('term')
          .apply(lambda g: pd.Series({
              'solar': wavg(g, 'solar'),   # MJ/m^2/day
              'temp':  wavg(g, 'temp'),    # °C
              'area_sum_km2': g['area_w'].sum()  # area sum contributed to the average
          }))
          .reset_index()
          .sort_values('term')
)

# create lag1 and lag2
df_grouped['solar_lag1'] = df_grouped['solar'].shift(1)
df_grouped['solar_lag2'] = df_grouped['solar'].shift(2)

df_grouped['temp_lag1'] = df_grouped['temp'].shift(1)
df_grouped['temp_lag2'] = df_grouped['temp'].shift(2)

df_grouped.head()

#output dataset
output_path = 'data/es_solar_temp.csv'
df_grouped.to_csv(output_path, index=False, encoding='utf-8-sig')