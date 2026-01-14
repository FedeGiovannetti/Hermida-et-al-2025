library(osfr)
library(gert)

# For github

git_add(".")

git_commit("Version 1.02. 
           - Changed 'Talk' label to 'Direct instruction' in all the code.
           - Created separate folders for png and tif figures.
           - Changed project name to 'plos_one_2026.proj")

git_push()

# For osf

node <- osf_retrieve_node("https://osf.io/hbdsc/")

r_files <- osf_upload(node, path = list.files(), conflicts = "overwrite")
