import numpy as np
import pandas as pd
from plotnine import *
from utils.projections import Projection


# =========================
# grid
# =========================
D_vals = np.linspace(-50, 50, 300)
ratio_vals = np.linspace(0, 5, 300)   # T1/T0

D_grid, ratio_grid = np.meshgrid(D_vals, ratio_vals)

# Projection with T0=1 so that T/T0 = ratio directly
proj = Projection(T0=1.0)
w_grid = proj.projection_w(D_grid, ratio_grid)

df_heat = pd.DataFrame({
    "D": D_grid.ravel(),
    "ratio": ratio_grid.ravel(),
    "w": w_grid.ravel(),
})


# =========================
# heat map
# =========================
p_heat = (
    ggplot(df_heat, aes(x="D", y="ratio", fill="w"))
    + geom_tile()
    + geom_vline(xintercept=0, linetype="solid", color="black", size=0.8)
    + scale_fill_distiller(type="div", palette="RdBu", name="w")
    + scale_x_continuous(expand=(0, 0))
    + scale_y_continuous(expand=(0, 0))
    + labs(
        title="Heatmap of w ",
        x="D",
        y="T₁ / T₀",
    )
    + theme_bw()
    + theme(
        plot_title=element_text(size=13, face="bold"),
        axis_title=element_text(size=11),
        axis_text=element_text(size=9),
        legend_title=element_text(size=10),
    )
)

p_heat.save("results/p_w_heatmap.png", dpi=300, width=18, height=14, units="cm")
