.onLoad <- function(libname, pkgname) {
  options("turbokit-snippetdir" = get_snippets_path())
  # options("turbokit-tabspref" = .rs.readUiPref("num_spaces_for_tab")) fails
  options("turbokit-smartpipe" = TRUE)
  # auto insert snippet?
  options("turbokit-autoinsert" = TRUE)
  # be chatty to users?
  options("turbokit-verbose" = TRUE)
  # attach package on boot() call? F because changing namespace is dangerous.
  options("turbokit-boot" = FALSE)
  # which prefix for which package in turbo()?
  options("turbokit-up" = {
    l <- vector(mode = "list", length = 9)
    names(l) <- letters[1:9]
    l
  })
}

.onAttach <- function(libname, pkgname) {
  # TODO better way of handling overwriting? user setting snippet name?
  if (identical(getOption("turbokit-snippetdir"), "error")) {
    return(NULL)
  }
  if ("s" %in% names(read_snippet(path = getOption("turbokit-snippetdir")))) {
    packageStartupMessage(
      "Snippet s detected\n",
      "CAUTION: verify this is not your personal snippet\n",
      "\n\t\t(Tools > Global Options > Code > Edit Snippets)\n",
      "\nThe %>>% operator will overwrite this on use."
    )
  }
}
