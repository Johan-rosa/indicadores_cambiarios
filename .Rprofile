if (require("here", quietly = TRUE)) {
  options(box.path = here())
}

if (Sys.getenv("ENV") == "BCRD") {
  logger::log_info("Setting proxy configuration. Check your credentials often.")
  box::use(scripts / logic / config_proxy[set_proxy])
  set_proxy()
}
