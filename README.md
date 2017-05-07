# heyexr
Import, transform, and visualize ocular coherence tomography (OCT) data in R

## Notes

* This software is for research purposes only and is not intended for diagnostic purposes.
* Heidelberg VOL file import functions (e.g., `read_heyex`) are based on the "Open Heyex Raw Files" plugin for ImageJ (https://imagej.nih.gov/ij/plugins/heyex/index.html) authored by Kris Sheets (Knott EJ, Sheets KG, Zhou Y, Gordon WC, Bazan NG.Spatial correlation of mouse photoreceptor-RPE thickness between SD-OCT and histology. Exp Eye Res. 2011 Feb;92(2):155-60. Epub 2010 Oct 28. PubMed ID: 21035444).

## Known issues

* Cannot currently parse date of birth from the VOL file. (DOB is encoded in the VOL file as a 64 bit integer, and R can't natively handle 64 bit numbers.
* Import and visualization functions has only been tested on volume scans. Radial scan patterns have not been tested.
