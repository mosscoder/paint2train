paint2train package
================

<img src="https://github.com/mosscoder/paint2train/blob/main/images/combo_banner.gif?raw=true" width="100%" />

## Background

The purpose of paint2train package is to rapidly label imagery and
spatial data at the pixel level. These labels may in turn be used to
train machine learning algorithms for tasks such as image segmentation.

There are currently four primary functions:

  - Generate tiles from larger contiguous sources of multi-spectral
    imagery or other spatial data
  - Pre-process tiles, generating normalized difference, edge detection,
    and neighborhood summary stats layers
  - Reduce n layers from pre-processing steps into 3 layers, using [UMAP
    dimension reduction methods](https://github.com/jlmelville/uwot)
  - Run a local Shiny app to rapidly label pixels based on dissimilarity
    thresholds (in UMAP space) to clicked points (labels are saved as
    .tifs as you work)

Here we provide an overview of the core app features.

## Data Labeling Process

Select imagery tiles from the dropdown menu found in the upper left.
<img src="https://github.com/mosscoder/paint2train/blob/main/images/image_select.gif?raw=true" width="100%" />

Click on a region you wish to classify and adjust the **Dissimilarity
Threshold** to match the extent of the class to label.
<img src="https://github.com/mosscoder/paint2train/blob/main/images/click_paint.gif?raw=true" width="100%" />

Select which class to label from the **Labeling Tools** menu, then click
the **Label painted areas** button to save the painted pixels to that
class. After painting and labeling focal areas, fill the remaining
unlabeled points by click the **FIll unlabeled as class** button.
<img src="https://github.com/mosscoder/paint2train/blob/main/images/label_class.gif?raw=true" width="100%" />

Adjust the color of painted areas from the **Aesthetics Controls** drop
down menu.
<img src="https://github.com/mosscoder/paint2train/blob/main/images/change_paint.gif?raw=true" width="100%" />

Manually edit pixels by using the draw tools (lower right). Draw a box
or polygon around the region you wish to edit, select the appropriate
class from the **Select class to label** menu, then click **Label drawn
areas**
<img src="https://github.com/mosscoder/paint2train/blob/main/images/draw.gif?raw=true" width="100%" />

Change the base imagery with the controls in the upper right.
<img src="https://github.com/mosscoder/paint2train/blob/main/images/base_select.gif?raw=true" width="100%" />

Filter high and low value outlier pixels to brighten or darken base
imagery layers by adjusting the **Baselayer quantiles** in the
**Aesthetics controls** drop down menu.
<img src="https://github.com/mosscoder/paint2train/blob/main/images/quantiles.gif?raw=true" width="100%" />

Click and drag to move the controls as needed.
<img src="https://github.com/mosscoder/paint2train/blob/main/images/move_controls.gif?raw=true" width="100%" />

## Additional Resources

Please visit the [paint2train package GitHub
repository](https://github.com/mosscoder/paint2train) for more details.
