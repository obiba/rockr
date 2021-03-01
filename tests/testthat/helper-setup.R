# Setup
options(rock.url = "http://localhost:8085")
#options(rock.url = "https://rock-demo.obiba.org")

check_skip <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if(is.null(getOption("rock.url")), "Skipping tests because Rock url is not defined")
}
