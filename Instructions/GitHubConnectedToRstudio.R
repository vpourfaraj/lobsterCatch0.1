# After  documenting a function run the follwoing code to regenrate .Rd file
devtools::document()


#To make Vignettes, run the follwing code which creates an Rmarkdown file. The templete file can then be used to provide instructions.
usethat::use_vignette("introduction")


#To Clone the package's GitHub repository to your computer via cmd (do it once; i.e. first time you create a package):
#locate the desired folder i.e. c: -> cd myFolder and run this code:
git clone https://github.com/vpourfaraj/lobsterCatch

# or

#Alternatively  you can  Clone the package's GitHub repository to your computer via RStudio
#File > New Project > Version Control > Git.
#paste: https://github.com/vpourfaraj/lobsterCatch.git

#-------------

# In oredr to keep an updated version of the codes locally and on github
# Open lobsterCatch.Rproj
#pull all changes from github and in case of any local changes commit and push changes to github



