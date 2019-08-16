
read_cppModel <- function(cppModel_file="", cppModel_name="test") {
  library(mrgsolve)
  #cppModel_file ="./KRM/cpp/mod_R1979_MM03_DAT0406.cpp"
  cppModel=mread(model=cppModel_name,project=dirname(cppModel_file),file=basename(cppModel_file))

return(cppModel)
}

