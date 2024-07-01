README - `raw_data`

This folder normally would contain all the excel files that were downloaded from Bluebike's servers in `project_builder_code.Rmd`. However, these files in their original form are HUGE! They could not all be backed up on Github within the 100 MB per file limit.

We saved each file as a compressed `.rds` file instead in the `/processing` folder. If you rerun our code, this folder should populate as expected with the excel files, but you're welcome to save yourselves time and just skip to using the compressed `.rds` files in `/processessing`.