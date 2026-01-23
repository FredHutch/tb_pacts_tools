"""
Data Profiler Module

Functions for generating data profile reports from CSV/TSV files.
Designed to be used with Quarto documents for automated reporting.
"""

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from pathlib import Path
from typing import Tuple, List, Dict, Any, Optional


def load_data(filepath: str) -> Tuple[pd.DataFrame, str]:
    """
    Auto-detect CSV or TSV format and load data with pandas.
    
    Parameters
    ----------
    filepath : str
        Path to the data file (CSV or TSV)
    
    Returns
    -------
    Tuple[pd.DataFrame, str]
        The loaded DataFrame and a string describing the detected format
    
    Raises
    ------
    FileNotFoundError
        If the specified file does not exist
    """
    path = Path(filepath)
    
    if not path.exists():
        raise FileNotFoundError(f"File not found: {filepath}")
    
    # Read first few lines to detect delimiter
    with open(path, 'r', encoding='utf-8', errors='replace') as f:
        sample = f.read(8192)
    
    # Count potential delimiters in sample
    comma_count = sample.count(',')
    tab_count = sample.count('\t')
    
    # Choose delimiter based on frequency
    if tab_count > comma_count:
        sep = '\t'
        detected_format = "TSV (tab-separated)"
    else:
        sep = ','
        detected_format = "CSV (comma-separated)"
    
    # Load the data
    df = pd.read_csv(filepath, sep=sep, low_memory=False)
    
    return df, detected_format


def infer_variable_type(series: pd.Series) -> str:
    """
    Infer the semantic type of a variable.
    
    Parameters
    ----------
    series : pd.Series
        The column to analyze
    
    Returns
    -------
    str
        One of: 'binary', 'categorical', 'discrete integer', 'continuous integer',
        'discrete float', 'continuous float', 'mixed (numeric as text)', 
        'datetime', 'text', 'empty', 'unknown'
    """
    # Drop missing values for type inference
    non_null = series.dropna()
    
    if len(non_null) == 0:
        return "empty"
    
    # Check if datetime
    if pd.api.types.is_datetime64_any_dtype(series):
        return "datetime"
    
    # Try to infer datetime from string
    if series.dtype == 'object':
        try:
            parsed = pd.to_datetime(non_null.head(100), format='mixed', dayfirst=False)
            # If successful on sample with few failures, likely datetime
            if parsed.notna().sum() / len(parsed) > 0.8 and len(non_null.unique()) > 10:
                return "datetime"
        except:
            pass
    
    # Check for numeric types
    if pd.api.types.is_numeric_dtype(series):
        unique_vals = non_null.unique()
        n_unique = len(unique_vals)
        
        # Binary: exactly 2 unique values
        if n_unique == 2:
            return "binary"
        
        # Check if integers
        if pd.api.types.is_integer_dtype(series) or \
           (pd.api.types.is_float_dtype(series) and (non_null == non_null.astype(int)).all()):
            # Discrete integer: few unique values relative to data size
            if n_unique <= 20 or (n_unique / len(non_null) < 0.05 and n_unique <= 50):
                return "discrete integer"
            else:
                return "continuous integer"
        else:
            # Float type
            if n_unique <= 20:
                return "discrete float"
            else:
                return "continuous float"
    
    # Object/string types
    if series.dtype == 'object' or pd.api.types.is_string_dtype(series):
        unique_vals = non_null.unique()
        n_unique = len(unique_vals)
        
        # Binary
        if n_unique == 2:
            return "binary"
        
        # Check if it's actually numeric stored as string
        try:
            numeric_series = pd.to_numeric(non_null, errors='coerce')
            if numeric_series.notna().sum() / len(non_null) > 0.9:
                return "mixed (numeric as text)"
        except:
            pass
        
        # Categorical vs text
        if n_unique <= 50 or (n_unique / len(non_null) < 0.1):
            return "categorical"
        else:
            return "text"
    
    # Boolean
    if pd.api.types.is_bool_dtype(series):
        return "binary"
    
    return "unknown"


def get_example_values(series: pd.Series, n: int = 3) -> str:
    """
    Get example values from the series as a formatted string.
    
    Parameters
    ----------
    series : pd.Series
        The column to sample from
    n : int, optional
        Number of examples to return (default 3)
    
    Returns
    -------
    str
        Comma-separated string of example values
    """
    non_null = series.dropna().unique()
    examples = non_null[:n]
    example_strs = [str(x)[:50] for x in examples]  # Truncate long strings
    return ", ".join(example_strs)


def get_overview_stats(df: pd.DataFrame, filepath: str, detected_format: str) -> Dict[str, Any]:
    """
    Calculate overview statistics for the dataset.
    
    Parameters
    ----------
    df : pd.DataFrame
        The loaded DataFrame
    filepath : str
        Path to the original file
    detected_format : str
        String describing the detected file format
    
    Returns
    -------
    Dict[str, Any]
        Dictionary containing overview statistics
    """
    n_observations = len(df)
    n_variables = len(df.columns)
    n_missing_cells = df.isnull().sum().sum()
    total_cells = n_observations * n_variables
    missing_pct = (n_missing_cells / total_cells * 100) if total_cells > 0 else 0
    n_duplicate_rows = df.duplicated().sum()
    memory_usage = df.memory_usage(deep=True).sum()
    
    # Format memory usage
    if memory_usage > 1e9:
        memory_str = f"{memory_usage / 1e9:.2f} GB"
    elif memory_usage > 1e6:
        memory_str = f"{memory_usage / 1e6:.2f} MB"
    elif memory_usage > 1e3:
        memory_str = f"{memory_usage / 1e3:.2f} KB"
    else:
        memory_str = f"{memory_usage} bytes"
    
    return {
        'filename': Path(filepath).name,
        'detected_format': detected_format,
        'n_observations': n_observations,
        'n_variables': n_variables,
        'n_missing_cells': n_missing_cells,
        'missing_pct': missing_pct,
        'n_duplicate_rows': n_duplicate_rows,
        'memory_str': memory_str
    }


def get_variable_info(df: pd.DataFrame) -> pd.DataFrame:
    """
    Build a table with metadata about each variable.
    
    Parameters
    ----------
    df : pd.DataFrame
        The loaded DataFrame
    
    Returns
    -------
    pd.DataFrame
        DataFrame with one row per variable containing metadata
    """
    var_info = []
    for col in df.columns:
        series = df[col]
        n_missing = series.isnull().sum()
        missing_pct = n_missing / len(series) * 100 if len(series) > 0 else 0
        n_unique = series.nunique()
        
        var_info.append({
            'Variable': col,
            'Pandas Dtype': str(series.dtype),
            'Inferred Type': infer_variable_type(series),
            'Unique Values': n_unique,
            'Missing (%)': f"{missing_pct:.1f}%",
            'Example Values': get_example_values(series)
        })
    
    return pd.DataFrame(var_info)


def get_numeric_summary(df: pd.DataFrame) -> Optional[pd.DataFrame]:
    """
    Generate summary statistics for numeric variables.
    
    Parameters
    ----------
    df : pd.DataFrame
        The loaded DataFrame
    
    Returns
    -------
    Optional[pd.DataFrame]
        DataFrame with summary statistics, or None if no numeric columns
    """
    numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
    
    if len(numeric_cols) == 0:
        return None
    
    # Get describe output and transpose for better display
    numeric_summary = df[numeric_cols].describe().T
    numeric_summary = numeric_summary.round(4)
    
    # Add additional statistics
    numeric_summary['missing'] = df[numeric_cols].isnull().sum()
    numeric_summary['missing_pct'] = (df[numeric_cols].isnull().sum() / len(df) * 100).round(2)
    numeric_summary['zeros'] = (df[numeric_cols] == 0).sum()
    numeric_summary['zeros_pct'] = ((df[numeric_cols] == 0).sum() / len(df) * 100).round(2)
    
    # Reorder columns
    col_order = ['count', 'missing', 'missing_pct', 'mean', 'std', 'min', '25%', '50%', '75%', 'max', 'zeros', 'zeros_pct']
    numeric_summary = numeric_summary[[c for c in col_order if c in numeric_summary.columns]]
    
    # Rename for clarity
    numeric_summary.columns = ['Count', 'Missing', 'Missing %', 'Mean', 'Std Dev', 
                               'Min', '25%', 'Median', '75%', 'Max', 'Zeros', 'Zeros %']
    numeric_summary.index.name = 'Variable'
    numeric_summary = numeric_summary.reset_index()
    
    return numeric_summary


def get_categorical_summary(df: pd.DataFrame) -> Optional[pd.DataFrame]:
    """
    Generate summary statistics for categorical variables.
    
    Parameters
    ----------
    df : pd.DataFrame
        The loaded DataFrame
    
    Returns
    -------
    Optional[pd.DataFrame]
        DataFrame with summary statistics, or None if no categorical columns
    """
    # Identify categorical columns (non-numeric)
    categorical_cols = df.select_dtypes(exclude=[np.number]).columns.tolist()
    
    # Also include numeric columns that are binary or have few unique values
    numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
    for col in numeric_cols:
        if df[col].nunique() <= 10:
            if col not in categorical_cols:
                categorical_cols.append(col)
    
    if len(categorical_cols) == 0:
        return None
    
    cat_summary = []
    for col in categorical_cols:
        series = df[col]
        n_unique = series.nunique()
        n_missing = series.isnull().sum()
        missing_pct = n_missing / len(series) * 100 if len(series) > 0 else 0
        
        # Get mode and its frequency
        value_counts = series.value_counts()
        if len(value_counts) > 0:
            mode_val = value_counts.index[0]
            mode_count = value_counts.iloc[0]
            mode_pct = mode_count / series.notna().sum() * 100 if series.notna().sum() > 0 else 0
        else:
            mode_val = "N/A"
            mode_count = 0
            mode_pct = 0
        
        # Get top categories
        top_cats = ", ".join([str(x)[:20] for x in value_counts.head(5).index])
        
        cat_summary.append({
            'Variable': col,
            'Unique Values': n_unique,
            'Missing': n_missing,
            'Missing %': f"{missing_pct:.1f}%",
            'Mode': str(mode_val)[:30],
            'Mode Count': mode_count,
            'Mode %': f"{mode_pct:.1f}%",
            'Top Categories': top_cats
        })
    
    return pd.DataFrame(cat_summary)


def plot_continuous(series: pd.Series, title: Optional[str] = None) -> plt.Figure:
    """
    Plot histogram for continuous/discrete numeric variables.
    
    Parameters
    ----------
    series : pd.Series
        The column to plot
    title : Optional[str]
        Plot title (default: "Distribution of {column_name}")
    
    Returns
    -------
    plt.Figure
        The matplotlib figure
    """
    if title is None:
        title = f"Distribution of {series.name}"
    
    fig, ax = plt.subplots(figsize=(10, 5))
    
    # Remove NaN for plotting
    data = series.dropna()
    
    if len(data) == 0:
        ax.text(0.5, 0.5, 'No data available', ha='center', va='center', transform=ax.transAxes)
        ax.set_title(title)
        plt.tight_layout()
        return fig
    
    # Determine number of bins
    n_unique = len(data.unique())
    if n_unique <= 30:
        bins = min(n_unique, 30)
    else:
        bins = 50
    
    sns.histplot(data=data, bins=bins, kde=True if n_unique > 10 else False, ax=ax, color='steelblue')
    ax.set_xlabel(series.name)
    ax.set_ylabel('Count')
    ax.set_title(title)
    
    # Add summary stats as text
    stats_text = f"n={len(data):,}  |  mean={data.mean():.2f}  |  median={data.median():.2f}  |  std={data.std():.2f}"
    ax.annotate(stats_text, xy=(0.5, -0.12), xycoords='axes fraction', ha='center', fontsize=9, color='gray')
    
    plt.tight_layout()
    return fig


def plot_categorical(series: pd.Series, title: Optional[str] = None, max_categories: int = 15) -> plt.Figure:
    """
    Plot bar chart for categorical variables showing proportions.
    
    Parameters
    ----------
    series : pd.Series
        The column to plot
    title : Optional[str]
        Plot title (default: "Distribution of {column_name}")
    max_categories : int
        Maximum number of categories to display (others grouped as "Other")
    
    Returns
    -------
    plt.Figure
        The matplotlib figure
    """
    if title is None:
        title = f"Distribution of {series.name}"
    
    fig, ax = plt.subplots(figsize=(10, 5))
    
    # Get value counts
    value_counts = series.value_counts()
    
    if len(value_counts) == 0:
        ax.text(0.5, 0.5, 'No data available', ha='center', va='center', transform=ax.transAxes)
        ax.set_title(title)
        plt.tight_layout()
        return fig
    
    # Limit categories if too many
    if len(value_counts) > max_categories:
        top_counts = value_counts.head(max_categories - 1)
        other_count = value_counts.iloc[max_categories - 1:].sum()
        value_counts = pd.concat([top_counts, pd.Series({'(Other)': other_count})])
    
    # Calculate proportions
    proportions = value_counts / value_counts.sum()
    
    # Create colors
    colors = sns.color_palette("husl", len(value_counts))
    
    # Create bar plot
    bars = ax.bar(range(len(value_counts)), proportions.values, color=colors)
    
    # Add percentage labels on bars
    for i, (bar, prop) in enumerate(zip(bars, proportions.values)):
        ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01, 
                f'{prop*100:.1f}%', ha='center', va='bottom', fontsize=8)
    
    # Set labels
    ax.set_xticks(range(len(value_counts)))
    labels = [str(x)[:20] + '...' if len(str(x)) > 20 else str(x) for x in value_counts.index]
    ax.set_xticklabels(labels, rotation=45, ha='right')
    ax.set_xlabel(series.name)
    ax.set_ylabel('Proportion')
    ax.set_title(title)
    ax.set_ylim(0, max(proportions.values) * 1.15)  # Add space for labels
    
    # Add legend
    legend_labels = [f"{labels[i]}: {value_counts.values[i]:,}" for i in range(len(value_counts))]
    ax.legend(bars, legend_labels, loc='upper right', fontsize=8, title='Category: Count')
    
    plt.tight_layout()
    return fig


def plot_datetime(series: pd.Series, title: Optional[str] = None) -> plt.Figure:
    """
    Plot histogram for datetime variables.
    
    Parameters
    ----------
    series : pd.Series
        The column to plot (will attempt datetime conversion)
    title : Optional[str]
        Plot title (default: "Distribution of {column_name}")
    
    Returns
    -------
    plt.Figure
        The matplotlib figure
    """
    if title is None:
        title = f"Distribution of {series.name}"
    
    fig, ax = plt.subplots(figsize=(10, 5))
    
    try:
        datetime_series = pd.to_datetime(series, errors='coerce')
        datetime_series.dropna().hist(bins=50, ax=ax, color='steelblue')
        ax.set_xlabel(series.name)
        ax.set_ylabel('Count')
        ax.set_title(title)
        plt.xticks(rotation=45)
        plt.tight_layout()
    except Exception as e:
        ax.text(0.5, 0.5, f'Could not plot: {e}', ha='center', va='center', transform=ax.transAxes)
        ax.set_title(title)
    
    return fig


def plot_text_wordcount(series: pd.Series, title: Optional[str] = None) -> plt.Figure:
    """
    Plot word count distribution for text variables.
    
    Parameters
    ----------
    series : pd.Series
        The text column to analyze
    title : Optional[str]
        Plot title (default: "Word Count Distribution for {column_name}")
    
    Returns
    -------
    plt.Figure
        The matplotlib figure
    """
    if title is None:
        title = f"Word Count Distribution for {series.name}"
    
    fig, ax = plt.subplots(figsize=(10, 5))
    
    try:
        word_counts = series.dropna().astype(str).str.split().str.len()
        sns.histplot(data=word_counts, bins=50, ax=ax, color='steelblue')
        ax.set_xlabel('Word Count')
        ax.set_ylabel('Frequency')
        ax.set_title(title)
        plt.tight_layout()
    except Exception as e:
        ax.text(0.5, 0.5, f'Could not plot: {e}', ha='center', va='center', transform=ax.transAxes)
        ax.set_title(title)
    
    return fig


def plot_variable(series: pd.Series, var_type: Optional[str] = None) -> Optional[plt.Figure]:
    """
    Plot a variable based on its inferred type.
    
    Parameters
    ----------
    series : pd.Series
        The column to plot
    var_type : Optional[str]
        The variable type (if None, will be inferred)
    
    Returns
    -------
    Optional[plt.Figure]
        The matplotlib figure, or None if plotting not applicable
    """
    if var_type is None:
        var_type = infer_variable_type(series)
    
    # Skip empty columns
    if series.dropna().empty:
        return None
    
    if var_type in ['continuous float', 'continuous integer', 'discrete integer', 'discrete float']:
        return plot_continuous(series)
    elif var_type in ['binary', 'categorical']:
        return plot_categorical(series)
    elif var_type == 'datetime':
        return plot_datetime(series)
    elif var_type == 'text':
        return plot_text_wordcount(series)
    elif var_type in ['mixed (numeric as text)', 'unknown']:
        # Try categorical if few unique values
        if series.nunique() <= 30:
            return plot_categorical(series)
    
    return None


def generate_all_plots(df: pd.DataFrame) -> List[Tuple[str, str, Optional[plt.Figure]]]:
    """
    Generate plots for all variables in the DataFrame.
    
    Parameters
    ----------
    df : pd.DataFrame
        The loaded DataFrame
    
    Returns
    -------
    List[Tuple[str, str, Optional[plt.Figure]]]
        List of tuples containing (column_name, inferred_type, figure)
    """
    plots = []
    for col in df.columns:
        series = df[col]
        var_type = infer_variable_type(series)
        fig = plot_variable(series, var_type)
        plots.append((col, var_type, fig))
    return plots

def get_numeric_correlations(df: pd.DataFrame, method: str = 'spearman') -> pd.DataFrame | None:
    """
    Compute correlation matrix for numeric variables.
    
    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe
    method : str
        Correlation method ('pearson', 'spearman', 'kendall')
    
    Returns
    -------
    pd.DataFrame or None
        Correlation matrix, or None if insufficient numeric columns
    """
    numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
    
    # Filter out columns that are all NaN or have zero variance
    valid_cols = []
    for col in numeric_cols:
        series = df[col].dropna()
        if len(series) > 1 and series.std() > 0:
            valid_cols.append(col)
    
    if len(valid_cols) < 2:
        return None
    
    return df[valid_cols].corr(method=method)


def get_categorical_correlations(df: pd.DataFrame, max_categories: int = 50) -> pd.DataFrame | None:
    """
    Compute Cramér's V association matrix for categorical variables.
    
    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe
    max_categories : int
        Maximum unique values for a column to be included
    
    Returns
    -------
    pd.DataFrame or None
        Association matrix, or None if insufficient categorical columns
    """
    # Identify categorical columns (including binary)
    cat_cols = []
    for col in df.columns:
        var_type = infer_variable_type(df[col])
        if var_type in ['binary', 'categorical'] and df[col].nunique() <= max_categories:
            cat_cols.append(col)
    
    if len(cat_cols) < 2:
        return None
    
    # Limit to reasonable number of columns for visualization
    if len(cat_cols) > 30:
        # Prioritize columns with fewer missing values
        missing_rates = {col: df[col].isnull().mean() for col in cat_cols}
        cat_cols = sorted(cat_cols, key=lambda x: missing_rates[x])[:30]
    
    # Compute Cramér's V for each pair
    n = len(cat_cols)
    corr_matrix = np.ones((n, n))
    
    for i in range(n):
        for j in range(i + 1, n):
            v = _cramers_v(df[cat_cols[i]], df[cat_cols[j]])
            corr_matrix[i, j] = v
            corr_matrix[j, i] = v
    
    return pd.DataFrame(corr_matrix, index=cat_cols, columns=cat_cols)


def _cramers_v(x: pd.Series, y: pd.Series) -> float:
    """
    Compute Cramér's V statistic for two categorical variables.
    
    Parameters
    ----------
    x, y : pd.Series
        Categorical variables
    
    Returns
    -------
    float
        Cramér's V value between 0 and 1
    """
    # Remove rows where either value is missing
    mask = x.notna() & y.notna()
    if mask.sum() < 2:
        return np.nan
    
    confusion_matrix = pd.crosstab(x[mask], y[mask])
    
    if confusion_matrix.size == 0:
        return np.nan
    
    chi2 = 0
    row_sums = confusion_matrix.sum(axis=1)
    col_sums = confusion_matrix.sum(axis=0)
    total = confusion_matrix.sum().sum()
    
    if total == 0:
        return np.nan
    
    for i in range(len(row_sums)):
        for j in range(len(col_sums)):
            expected = row_sums.iloc[i] * col_sums.iloc[j] / total
            if expected > 0:
                observed = confusion_matrix.iloc[i, j]
                chi2 += (observed - expected) ** 2 / expected
    
    n = total
    min_dim = min(len(row_sums), len(col_sums)) - 1
    
    if min_dim == 0 or n == 0:
        return np.nan
    
    return np.sqrt(chi2 / (n * min_dim))


def get_missing_matrix(df: pd.DataFrame) -> pd.DataFrame:
    """
    Create a boolean matrix indicating missing values.
    
    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe
    
    Returns
    -------
    pd.DataFrame
        Boolean dataframe where True indicates missing
    """
    return df.isnull()


def get_missing_correlations(df: pd.DataFrame, min_missing_rate: float = 0.01) -> pd.DataFrame | None:
    """
    Compute correlations between missing value indicators.
    
    This shows which variables tend to be missing together.
    
    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe
    min_missing_rate : float
        Minimum proportion of missing values for a column to be included
    
    Returns
    -------
    pd.DataFrame or None
        Correlation matrix of missingness indicators
    """
    # Get columns with missing values above threshold
    missing_rates = df.isnull().mean()
    cols_with_missing = missing_rates[
        (missing_rates >= min_missing_rate) & (missing_rates < 1.0)
    ].index.tolist()
    
    if len(cols_with_missing) < 2:
        return None
    
    # Limit columns for readability
    if len(cols_with_missing) > 40:
        # Sort by missing rate and take a mix
        sorted_cols = missing_rates[cols_with_missing].sort_values()
        cols_with_missing = sorted_cols.index[:40].tolist()
    
    # Create missing indicator matrix and compute correlation
    missing_indicators = df[cols_with_missing].isnull().astype(int)
    
    return missing_indicators.corr()


def plot_correlation_heatmap(
    corr_matrix: pd.DataFrame,
    title: str,
    cmap: str = "RdBu_r",
    center: float | None = None,
    figsize: tuple | None = None
) -> plt.Figure:
    """
    Plot a correlation matrix as a heatmap.
    
    Parameters
    ----------
    corr_matrix : pd.DataFrame
        Correlation matrix to plot
    title : str
        Plot title
    cmap : str
        Colormap name
    center : float or None
        Value to center the colormap on
    figsize : tuple or None
        Figure size (width, height). Auto-calculated if None.
    
    Returns
    -------
    plt.Figure
        The figure object
    """
    n = len(corr_matrix)
    
    # Auto-calculate figure size
    if figsize is None:
        size = max(8, min(n * 0.4, 16))
        figsize = (size, size * 0.85)
    
    fig, ax = plt.subplots(figsize=figsize)
    
    # Create mask for upper triangle (optional, for cleaner look)
    mask = np.triu(np.ones_like(corr_matrix, dtype=bool), k=1)
    
    # Determine vmin/vmax
    if center is not None:
        vmax = max(abs(corr_matrix.min().min()), abs(corr_matrix.max().max()))
        vmin = -vmax
    else:
        vmin, vmax = 0, 1
    
    # Plot heatmap
    im = ax.imshow(
        corr_matrix.values,
        cmap=cmap,
        aspect='auto',
        vmin=vmin,
        vmax=vmax
    )
    
    # Add colorbar
    cbar = plt.colorbar(im, ax=ax, shrink=0.8)
    
    # Set ticks and labels
    ax.set_xticks(range(n))
    ax.set_yticks(range(n))
    
    # Truncate long labels
    max_label_len = 15 if n > 20 else 20
    xlabels = [s[:max_label_len] + '...' if len(s) > max_label_len else s 
               for s in corr_matrix.columns]
    ylabels = [s[:max_label_len] + '...' if len(s) > max_label_len else s 
               for s in corr_matrix.index]
    
    ax.set_xticklabels(xlabels, rotation=45, ha='right', fontsize=8)
    ax.set_yticklabels(ylabels, fontsize=8)
    
    ax.set_title(title, fontsize=12, fontweight='bold')
    
    plt.tight_layout()
    return fig


def plot_missing_heatmap(
    df: pd.DataFrame,
    max_cols: int = 50,
    max_rows: int = 500,
    figsize: tuple | None = None
) -> plt.Figure | None:
    """
    Plot a heatmap showing missing value patterns.
    
    Parameters
    ----------
    df : pd.DataFrame
        Input dataframe
    max_cols : int
        Maximum number of columns to display
    max_rows : int
        Maximum number of rows to sample for display
    figsize : tuple or None
        Figure size
    
    Returns
    -------
    plt.Figure or None
        The figure object, or None if no missing values
    """
    # Get columns with missing values, sorted by missing rate
    missing_rates = df.isnull().mean()
    cols_with_missing = missing_rates[missing_rates > 0].sort_values(ascending=False)
    
    if len(cols_with_missing) == 0:
        return None
    
    # Limit columns
    cols_to_plot = cols_with_missing.head(max_cols).index.tolist()
    
    # Sample rows if necessary
    if len(df) > max_rows:
        sample_idx = np.random.choice(len(df), max_rows, replace=False)
        sample_idx = np.sort(sample_idx)
        plot_df = df.iloc[sample_idx][cols_to_plot]
    else:
        plot_df = df[cols_to_plot]
    
    # Create missing indicator matrix
    missing_matrix = plot_df.isnull()
    
    # Calculate figure size
    if figsize is None:
        width = max(10, min(len(cols_to_plot) * 0.3, 16))
        height = max(4, min(len(plot_df) * 0.02, 10))
        figsize = (width, height)
    
    fig, ax = plt.subplots(figsize=figsize)
    
    # Create custom colormap (white for present, colored for missing)
    cmap = plt.cm.colors.ListedColormap(['#f0f0f0', '#e74c3c'])
    
    # Plot
    im = ax.imshow(
        missing_matrix.values.astype(int),
        aspect='auto',
        cmap=cmap,
        interpolation='nearest'
    )
    
    # Add column labels
    ax.set_xticks(range(len(cols_to_plot)))
    max_label_len = 12 if len(cols_to_plot) > 30 else 18
    xlabels = [s[:max_label_len] + '..' if len(s) > max_label_len else s 
               for s in cols_to_plot]
    ax.set_xticklabels(xlabels, rotation=45, ha='right', fontsize=8)
    
    # Remove y-axis labels (row indices not meaningful)
    ax.set_yticks([])
    ax.set_ylabel(f"Observations (n={len(plot_df)})", fontsize=10)
    
    # Add missing rate annotation at top
    for i, col in enumerate(cols_to_plot):
        rate = missing_rates[col] * 100
        ax.text(i, -0.5, f'{rate:.0f}%', ha='center', va='bottom', fontsize=7, rotation=90)
    
    ax.set_title("Missing Value Patterns (red = missing)", fontsize=12, fontweight='bold')
    
    # Add legend
    from matplotlib.patches import Patch
    legend_elements = [
        Patch(facecolor='#f0f0f0', edgecolor='gray', label='Present'),
        Patch(facecolor='#e74c3c', edgecolor='gray', label='Missing')
    ]
    ax.legend(handles=legend_elements, loc='upper right', bbox_to_anchor=(1.15, 1))
    
    plt.tight_layout()
    return fig