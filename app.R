# using Joe Cheng's Posit Conf slides
#   https://jcheng5.github.io/posit-conf-2023-shinylive/#/title-slide
#   and Max Kuhn's example https://topepo.github.io/shinylive-in-book-test/
# try to recreate https://www.jsinr.me/2023/09/25/gics-tree/
#   in shinylive to go from GICS Excel to checkbox tree

library(shiny)

webr::install("readxl")
webr::install("tidyr")
webr::install("dplyr")
webr::install("htmlwidgets")
library(readxl)
library(tidyr)
library(dplyr)
library(htmlwidgets)


query_runiverse <- function(package, rver = "4.3.0") {
  # to leverage r-universe we will need jsonlite
  #   but of course this comes at a cost to speed
  #   do not use httr to avoid another dependency
  webr::install("jsonlite")
  library(jsonlite)

  # find package
  # pkg_summary <- jsonlite::fromJSON(paste0("https://r-universe.dev/stats/powersearch?limit=1&all=true&q=",package), simplifyVector = FALSE)[[1]]
  # author <- pkg_summary[[1]]$maintainer$login
  # package <- pkg_summary[[1]]$Package
    author <- "timelyportfolio"

  # get package details
  pkg_details <- jsonlite::fromJSON(
    paste0(
      "https://",
      author,
      ".r-universe.dev/",
      package,
      "/json"
    ),
    simplifyVector = FALSE
  )

  # mac binary information
  wasm_binary <- Filter(function(binary){grepl(x=binary$r,pattern=paste0("^",rver)) && binary$os == "wasm"},pkg_details$`_binaries`)[[1]]

  list(
    author = author,
    version = wasm_binary$version,
    needs_compilation = pkg_details$NeedsCompilation
  )
}

install_runiverse <- function(packages, lib = NULL) {
  # most of this code copied from webr install function
  # https://github.com/r-wasm/webr/blob/8c1c8038e4d238e91ec141537de11e114a01da2b/packages/webr/R/install.R

  if (is.null(lib)) {
    lib <- .libPaths()[[1]]
  }
  # only works for r-universe

  for (pkg in packages) {
    # ugly with strsplit instead of gsub but I gave up on more elegant solution
    ver <- as.character(getRversion())
    ver_split <- strsplit(ver, ".", fixed = TRUE)
    ver <- sprintf("%s.%s", ver_split[[1]][1], ver_split[[1]][2])

    pkg_info <- query_runiverse(package = pkg, rver = ver)

    author <- pkg_info$author
    pkg_ver <- pkg_info$version
    # should probably add a fail point here if needs_compilation != no

    bin_suffix <- sprintf("bin/macosx/contrib/%s",ver)
    
    repo = sprintf("https://%s.r-universe.dev", author)
    
    path <- file.path(repo, bin_suffix, paste0(pkg, "_", pkg_ver, ".tgz"))

    tmp <- tempfile()
    message(paste("Installing webR package:", pkg, " from r-universe"))
    utils::download.file(path, tmp, quiet = TRUE)

    utils::untar(
      tmp,
      exdir = lib,
      tar = "internal",
      extras = "--no-same-permissions"
    )
  }
  invisible(NULL)
}

install_runiverse(packages="shinyTree")
library(shinyTree)

ui <- shinyTree::shinyTree(
  "gicstree",
  checkbox = TRUE,
  theme = "proton",
  themeIcons = FALSE,
  themeDots = FALSE
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    # use temp file for this example but save to somewhere more permanent
    #   if you plan to reuse
    gics_file_english <- tempfile(fileext=".xlsx")
    
    # download the Excel file with the current GICS structure
    download.file(
      url = "https://www.spglobal.com/spdji/en/documents/index-policies/2023-gics-structure-english.xlsx",
      destfile = gics_file_english,
      mode = "wb"
    )
    
    # read the Excel file into a data.frame
    gics_english <- readxl::read_xlsx(
      path = gics_file_english,
      sheet = "Effective close of Mar 17 2023",
      skip = 3
    )

    remove_gics_comments <- function(x) {
      # remove anything in parenthesis, such as New Code, Discontinued, etc.
      gsub(x = x, pattern = "\\s\\(.*\\)", replacement = "")
    }
    
    gics <- gics_english %>%
      # delete discontinued
      dplyr::filter(
        dplyr::if_all(
          everything(),
          ~!grepl(x=.x, pattern = "(Discontinued)")
        )
      ) %>%
      # change column names
      dplyr::select(
        GICS4 = `...8`,
        GICS4_Code = `Sub-Industry`,
        GICS3_Code = `Industry`,
        GICS2_Code = `Industry Group`,
        GICS1_Code = Sector
      ) %>%
      # convert all codes to double (numeric)
      dplyr::mutate(
        dplyr::across(
          dplyr::contains("Code"),
          as.double
        )
      ) %>%
      # remove na rows for GICS4_Code which are description rows
      dplyr::filter(!is.na(GICS4_Code)) %>%
      # fill down in each of the Code columns to help us build hierarchy
      tidyr::fill(
        GICS3_Code,
        .direction = "down"
      ) %>%
      tidyr::fill(
        GICS2_Code,
        .direction = "down"
      ) %>%
      tidyr::fill(
        GICS1_Code,
        .direction = "down"
      ) %>%
      # clean
      dplyr::mutate(GICS4 = remove_gics_comments(GICS4))

    gics1 <- gics_english %>%
      # delete discontinued
      dplyr::filter(
        dplyr::if_all(
          1:2,
          ~!grepl(x=.x, pattern = "(Discontinued)")
        )
      ) %>%
      filter(!is.na(as.double(Sector))) %>%
      select(GICS1_Code = 1, GICS1_EN = 2) %>%
      unique() %>%
      dplyr::mutate(
        dplyr::across(
          2,
          remove_gics_comments
        )
      )
    
    gics2 <- gics_english %>%
      # delete discontinued
      dplyr::filter(
        dplyr::if_all(
          1:4,
          ~!grepl(x=.x, pattern = "(Discontinued)")
        )
      ) %>%
      select(GICS2_Code = 3, GICS2_EN = 4) %>%
      filter(!is.na(as.double(GICS2_Code))) %>%
      unique() %>%
      dplyr::mutate(
        dplyr::across(
          2,
          remove_gics_comments
        )
      )
    
    gics3 <- gics_english %>%
      # delete discontinued
      dplyr::filter(
        dplyr::if_all(
          1:6,
          ~!grepl(x=.x, pattern = "(Discontinued)")
        )
      ) %>%
      select(GICS3_Code = 5, GICS3_EN = 6) %>%
      filter(!is.na(as.double(GICS3_Code))) %>%
      unique() %>%
      dplyr::mutate(
        dplyr::across(
          2,
          remove_gics_comments
        )
      )
    
    gics4 <- gics_english %>%
      # delete discontinued
      dplyr::filter(
        dplyr::if_all(
          everything(),
          ~!grepl(x=.x, pattern = "(Discontinued)")
        )
      ) %>%
      select(GICS4_Code = 7, GICS4_EN = 8) %>%
      filter(!is.na(as.double(GICS4_Code))) %>%
      unique() %>%
      dplyr::mutate(
        dplyr::across(
          2,
          remove_gics_comments
        )
      )

    df <- gics  %>%
      select(-GICS4) %>%
      {
        sorted <- sort(colnames(.))
        select(., all_of(sorted))
      } %>%
      mutate(across(everything(),as.character))
    cols <- colnames(df)
    while(length(cols) > 0) {
      print(cols)
      lastcol <- tail(cols,1)
      gicslvl <- gsub(x=lastcol,pattern="_Code",replacement="")
      ref <- get(tolower(gicslvl))
      if(gicslvl != "GICS4") { # nest levels 1-3
        nestcols <- colnames(df)[which(!(colnames(df) %in% cols))]
        df <- nest(df, children := c(code,nestcols))
      }
      df <- right_join(df, ref)
      df <- rename(df, code = !!paste0(gicslvl,"_Code"))
      # important step which makes the property text from the English name
      df <- mutate(df, text = df[paste0(gicslvl,"_EN")][[1]])
      cols <- head(cols, length(cols) - 1)
    }
    
    gics_json <- jsonlite::toJSON(df, auto_unbox=TRUE)

    shinyTree::updateTree(
        session = session,
        treeId = "gicstree",
        data = unclass(gics_json)
    )
    
    observeEvent(shinyTree::get_selected_nodes(input$gicstree), {
        print(shinyTree::get_selected_nodes(input$gicstree))
    })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
