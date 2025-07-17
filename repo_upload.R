library(osfr)
library(gert)

# For github

git_add(".")

git_commit("Version 1.01. added tables in excel format and figures in tiff")

git_push()

# For osf

node <- osf_retrieve_node("https://osf.io/hbdsc/")

r_files <- osf_upload(node, path = list.files(),conflicts = "overwrite")
