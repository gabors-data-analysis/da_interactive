#!/usr/bin/env python3
import os
import sys
import traceback

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

def convert_csv_to_parquet(csv_path, out_dir, parquet_name=None):
    import pandas as pd

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
