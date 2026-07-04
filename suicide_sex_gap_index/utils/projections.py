from __future__ import annotations

from dataclasses import dataclass
from typing import Union
import numpy as np

ArrayLike = Union[float, int, np.ndarray]


@dataclass(slots=True)
class Projection:
    """Construct w from D, T, and T0."""

    T0: float

    def __post_init__(self) -> None:
        self.T0 = float(self.T0)
        if self.T0 <= 0.0:
            raise ValueError("T0 must be strictly positive")

    def projection_w(
        self,
        D: ArrayLike,
        T: ArrayLike,
        T0: float | None = None,
    ) -> ArrayLike:
        """
        Construct w:
            w = sgn*(D) * (|D| + 1) * (T / T0)

        where sgn*(D) is +1 if D >= 0, -1 if D < 0.
        Therefore, when D = 0, w = T / T0.
        """
        t0 = self.T0 if T0 is None else float(T0)
        if t0 <= 0.0:
            raise ValueError("T0 must be strictly positive")

        D_arr = np.asarray(D)
        T_arr = np.asarray(T)

        sign = np.where(D_arr >= 0, 1.0, -1.0)

        return sign * (np.abs(D_arr) + 1.0) * (T_arr / t0)
