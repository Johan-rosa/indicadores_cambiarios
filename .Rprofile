if (require("here", quietly = TRUE)) {
  options(box.path = here())
}

if (Sys.getenv("ENV") == "BCRD") {
  box::use(scripts / logic / config_proxy[set_proxy])
  set_proxy()
}
