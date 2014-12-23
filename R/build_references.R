R.utils::use("R.rsp")

local({
  opwd <- setwd("assets/references/")
  on.exit(setwd(opwd))
  rfile("references.dynamic.bib.rsp")
  rfile("references.bib.rsp")
})
