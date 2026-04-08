#!/usr/bin/env python3
import os
import sys
import traceback

import numpy as np
import pandas as pd

try:
    import tkinter as tk
    from tkinter import filedialog, messagebox
except Exception:
    print("Error: tkinter is required for the file chooser dialogs.")
    print("On Linux, you may need to install it via your package manager (e.g. python3-tk).")
    sys.exit(1)


YEAR_MIN = 2015
YEAR_MAX = 2023

COUNTY_MAP = {
    1: "Budapest",
    2: "Baranya",
    3: "BĂˇcs-Kiskun",
    4: "BĂ©kĂ©s",
    5: "Borsod-AbaĂşj-ZemplĂ©n",
    6: "CsongrĂˇd-CsanĂˇd",
    7: "FejĂ©r",
    8: "GyĹ‘r-Moson-Sopron",
    9: "HajdĂş-Bihar",
    10: "Heves",
    11: "KomĂˇrom-Esztergom",
    12: "NĂłgrĂˇd",
    13: "Pest",
    14: "Somogy",
    15: "Szabolcs-SzatmĂˇr-Bereg",
    16: "JĂˇsz-Nagykun-Szolnok",
    17: "Tolna",
    18: "Vas",
    19: "VeszprĂ©m",
    20: "Zala",
}

EXPORT_COLUMNS = [
    "id",
    "year",
    "emp",
    "sales_clean23",
    "received_grant",
    "foundyear",
    "county",
    "firm_owner",
    "export23",
    "tanass_clean23",
    "pretax23",
    "ereduzem23",
    "persexp_clean23",
    "eszk23",
    "ranyag23",
    "rlk23",
    "hlk23",
    "jetok23",
    "grant_value",
    "nace2_in_2015",
]


def ensure_deps():
    try:
        import pandas as pd  # noqa: F401
    except Exception:
        messagebox.showerror(
            "Missing dependency",
            "The 'pandas' package is required.\nInstall with:\n\n    pip install pandas pyarrow",
        )
        sys.exit(1)

    try:
        import pyarrow  # noqa: F401
    except Exception:
        messagebox.showerror(
            "Missing dependency",
            "The 'pyarrow' package is required to write Parquet.\nInstall with:\n\n    pip install pyarrow",
        )
        sys.exit(1)


def pick_csv_file(root):
    return filedialog.askopenfilename(
        title="Choose a CSV file",
        filetypes=[("CSV files", "*.csv"), ("All files", "*.*")],
    )


def pick_output_folder(root):
    return filedialog.askdirectory(title="Choose output folder")


def infer_parquet_name(csv_path):
    base = os.path.basename(csv_path)
    name, _ext = os.path.splitext(base)
    return f"{name}.parquet"


def read_csv(csv_path):
    try:
        return pd.read_csv(csv_path)
    except Exception as original_error:
        try:
            return pd.read_csv(csv_path, sep=None, engine="python")
        except Exception:
            raise original_error


def prepare_panel_data(df, year_min=YEAR_MIN, year_max=YEAR_MAX):
    """Apply the panel dataprep steps before writing the Parquet file."""
    panel = df.copy()

    # Match the original scale conversion.
    panel["grant_value"] = panel["grant_value"] / 1_000

    # Keep the target years and one row per firm-year.
    years = np.arange(year_min, year_max + 1)
    panel = panel.loc[panel["year"].between(year_min, year_max)].copy()
    panel = (
        panel.sort_values(["id", "year"])
        .drop_duplicates(["id", "year"], keep="first")
    )

    # Keep only firms observed in every year of the panel.
    ids_complete = (
        panel.groupby("id")["year"]
        .nunique()
        .pipe(lambda s: s[s == len(years)].index)
    )
    panel = panel.loc[panel["id"].isin(ids_complete)].copy()

    # Keep only firms whose 2-digit TEAOR code is stable over time.
    panel = (
        panel.sort_values(["id", "year"])
        .groupby("id", as_index=False, group_keys=False)
        .filter(lambda g: g["teaor08_2d"].dropna().nunique() <= 1)
        .copy()
    )

    # Replace county codes with county names, then keep the export schema.
    panel["county"] = panel["county"].map(COUNTY_MAP)
    return panel[EXPORT_COLUMNS].copy()


def convert_csv_to_parquet(csv_path, out_dir, parquet_name=None):
    if not parquet_name:
        parquet_name = infer_parquet_name(csv_path)

    df = read_csv(csv_path)
    panel = prepare_panel_data(df)

    out_path = os.path.join(out_dir, parquet_name)
    panel.to_parquet(out_path, index=False)
    return out_path


def main():
    ensure_deps()

    root = tk.Tk()
    root.withdraw()
    root.update()

    csv_path = pick_csv_file(root)
    if not csv_path:
        messagebox.showinfo("Canceled", "No CSV selected. Exiting.")
        return

    out_dir = pick_output_folder(root)
    if not out_dir:
        messagebox.showinfo("Canceled", "No output folder selected. Exiting.")
        return

    suggested = infer_parquet_name(csv_path)
    parquet_path = filedialog.asksaveasfilename(
        title="Save Parquet as...",
        initialdir=out_dir,
        initialfile=suggested,
        defaultextension=".parquet",
        filetypes=[("Parquet files", "*.parquet")],
    )
    if not parquet_path:
        messagebox.showinfo("Canceled", "No output file chosen. Exiting.")
        return

    try:
        out_path = convert_csv_to_parquet(
            csv_path,
            os.path.dirname(parquet_path),
            os.path.basename(parquet_path),
        )
        messagebox.showinfo("Success", f"Parquet file created:\n{out_path}")
    except Exception as e:
        traceback_str = "".join(traceback.format_exception(type(e), e, e.__traceback__))
        messagebox.showerror(
            "Conversion failed",
            f"An error occurred:\n\n{e}\n\nDetails:\n{traceback_str}",
        )


if __name__ == "__main__":
    main()
