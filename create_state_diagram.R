# a pretty fun application of ChatGPT
# creating the current state diagram from the code itself.

extract_state_transitions <- function(text) {

  if (length(text) > 1)
    text <- paste(text, collapse = "\n")

  lines <- strsplit(text, "\n")[[1]]

  current_state <- NULL
  edges <- data.frame(
    from = character(),
    to = character(),
    stringsAsFactors = FALSE
  )

  for (line in lines) {

    ## entering a state block
    m <- regexec(
      "if\\s*\\(\\s*SEIR_status\\s*==\\s*([0-9]+)\\s*\\)",
      line
    )

    x <- regmatches(line, m)[[1]]

    if (length(x) > 1)
      current_state <- x[2]

    ## state transition
    m2 <- regexec(
      "SEIR_status\\s*=\\s*([0-9]+)",
      line
    )

    y <- regmatches(line, m2)[[1]]

    if (length(y) > 1 && !is.null(current_state)) {

      edges <- rbind(
        edges,
        data.frame(
          from = current_state,
          to = y[2]
        )
      )
    }
  }

  unique(edges)
}

txt <- readLines("get_timeseries.cpp")

edges <- extract_state_transitions(txt)
# *****
# remove the last
edges <- edges[1:(nrow(edges) -1), ]
# *****

library(DiagrammeR)

plot_state_machine <- function(edges) {

  labels <- c(
    "0" = "Susceptible",
    "1" = "Exposed",
    "2" = "Infected",
    "3" = "Recovered"
  )

  node_text <- paste0(
    labels[names(labels)],
    collapse = "; "
  )

  edge_text <- paste(
    apply(edges, 1, function(x)
      sprintf("%s -> %s", labels[x[1]], labels[x[2]])),
    collapse = "; "
  )

  grViz(sprintf("
  digraph {
      graph [layout=dot]

      %s;

      %s;
  }
  ",
  node_text,
  edge_text))
}

library(DiagrammeRsvg)
library(rsvg)

g <- plot_state_machine(edges)

svg <- export_svg(g)

rsvg_png(
  charToRaw(svg),
  file = "diagram.png",
  width = 1200,
  height = 800
)
