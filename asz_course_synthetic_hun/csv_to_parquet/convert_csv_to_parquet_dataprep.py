#!/usr/bin/env python3
import os
import sys
import traceback
import numpy as np
import pandas as pd
from pathlib import Path

# Try to import GUI + data deps up front so we can show helpful messages
try:
    import tkinter as tk
    from tkinter import filedialog, messagebox
except Exception as e:
    print("Error: tkinter is required for the file chooser dialogs.")
    print("On Linux, you may need to install it via your package manager (e.g. python3-tk).")
    sys.exit(1)

def ensure_deps():
    try:
        import pandas as pd  # noqa: F401
    except Exception:
        messagebox.showerror(
            "Missing dependency",
            "The 'pandas' package is required.\nInstall with:\n\n    pip install pandas pyarrow"
        )
        sys.exit(1)
    try:
        import pyarrow  # noqa: F401
    except Exception:
        # pandas can sometimes write parquet with fastparquet too, but we recommend pyarrow
        messagebox.showerror(
            "Missing dependency",
            "The 'pyarrow' package is required to write Parquet.\nInstall with:\n\n    pip install pyarrow"
        )
        sys.exit(1)

def pick_csv_file(root):
    return filedialog.askopenfilename(
        title="Choose a CSV file",
        filetypes=[("CSV files", "*.csv"), ("All files", "*.*")]
    )

def pick_output_folder(root):
    return filedialog.askdirectory(title="Choose output folder")

def infer_parquet_name(csv_path):
    base = os.path.basename(csv_path)
    name, _ext = os.path.splitext(base)
    return f"{name}.parquet"

BASE_DIR = Path(__file__).resolve().parent.parent
DATA_DIR = BASE_DIR / "data"

COUNTY_PATH = DATA_DIR / "county_names_codes.xlsx"
NACE2_PATH = DATA_DIR / "nace2_labels.xlsx"

def repeat_cleaning(df, error_message=None):
    try:
        df = df.rename(columns={"id":"row_id",
                            "teaor08_2d":"nace2",
                            "export":"export_value",
                            "rlk":"liabilities",
                            "received_grant":"has_grant",
                            "firm_exit":"exit",
                            "received_grant_2020":"has_grant_2020"})
        
        df["ln_sales"] = np.log(np.clip(df["sales_clean"].astype(float), 1e-9, None))

        df["age"] = 2019 - df["foundyear"]

        df['has_export'] = df.apply(lambda x: 1 if x['export_value'] > 0 and  x['export_value'] != np.nan else 0, axis=1)

        county_name_correspondances = pd.read_excel(COUNTY_PATH)
        county_name_correspondances = county_name_correspondances.rename(columns={"CODE":"county","NAME":"county_name"})

        df = df.merge(county_name_correspondances,how="left",on="county")

        lab = pd.read_excel(NACE2_PATH)
        lab["nace2"] = lab["nace2"].apply(str)
        lab["nace2"] = lab["nace2"].str.extract(r"(\d+)", expand=False).fillna("").str.zfill(2).str[:2]

        df['nace2'] = df['nace2'].astype('Int64').astype(str).str.zfill(2)

        df = df.merge(lab, on="nace2", how="left")
        df["nace2_name_code"] = df["name_hu"].fillna("NACE " + df["nace2"]) + " (" + df["nace2"] + ")"

        return df

    except Exception as e:
        msg = error_message or "repeat_cleaning() failed, continuing without cleaning."
        print(f"{msg} Error details: {e}")
        return df


def convert_csv_to_parquet(csv_path, out_dir, parquet_name=None):
    

    if not parquet_name:
        parquet_name = infer_parquet_name(csv_path)

    out_path = os.path.join(out_dir, parquet_name)

    # Read CSV with sensible defaults; pandas infers delimiter by default if sep=None (>=2.0)
    # For older pandas versions, you could set sep=None and engine="python".
    try:
        df = pd.read_csv(csv_path)


    except Exception as e:
        # Try a fallback with more permissive parser if needed
        try:
            df = pd.read_csv(csv_path, sep=None, engine="python")
        except Exception:
            raise e

    df = repeat_cleaning(df)

    # Write Parquet (compression='snappy' is common; change if desired)
    df.to_parquet(out_path, index=False)  # uses pyarrow by default if installed
    return out_path

def main():
    ensure_deps()

    root = tk.Tk()
    root.withdraw()  # hide main window, just show dialogs
    root.update()

    csv_path = pick_csv_file(root)
    if not csv_path:
        messagebox.showinfo("Canceled", "No CSV selected. Exiting.")
        return

    out_dir = pick_output_folder(root)
    if not out_dir:
        messagebox.showinfo("Canceled", "No output folder selected. Exiting.")
        return

    # Optional: let the user confirm/rename the parquet filename
    suggested = infer_parquet_name(csv_path)
    parquet_path = filedialog.asksaveasfilename(
        title="Save Parquet as...",
        initialdir=out_dir,
        initialfile=suggested,
        defaultextension=".parquet",
        filetypes=[("Parquet files", "*.parquet")]
    )
    if not parquet_path:
        messagebox.showinfo("Canceled", "No output file chosen. Exiting.")
        return

    try:
        out_path = convert_csv_to_parquet(csv_path, os.path.dirname(parquet_path), os.path.basename(parquet_path))
        messagebox.showinfo("Success", f"Parquet file created:\n{out_path}")
    except Exception as e:
        traceback_str = "".join(traceback.format_exception(type(e), e, e.__traceback__))
        messagebox.showerror("Conversion failed", f"An error occurred:\n\n{e}\n\nDetails:\n{traceback_str}")

if __name__ == "__main__":
    main()
