
#' @import (dplyr)
#' @import (dm)
#' @import (DiagrammeR)
showRelationship <- function(data_list) {


# Create the data model
data_model <- dm(
  pedestrians = data_list$pedestrians,
  incidents = data_list$incidents,
  drivers = data_list$drivers
)

# Add primary keys
data_model <- data_model %>%
  dm_add_pk(pedestrians, `Report Number`) %>%
  dm_add_pk(drivers, `Report Number`) %>%
  dm_add_pk(incidents, `Report Number`)
# Identify common columns between the tables
common1 <-
  colnames(drivers)[colnames(drivers)
                                    %in%
                                      colnames(pedestrians)]
common2 <-
  colnames(incidents)[colnames(incidents)
                                             %in%
                                               colnames(drivers)]


#exclude PK
common1 <- common1[common1 != "Report Number"]
common2 <- common2[common2 != "Report Number"]
# Add foreign key relationships for each common column

for (col in common1) {
  data_model <- data_model %>%
    dm_add_fk(drivers,
              !!col,
              pedestrians,
              !!col)
}


for (col in common2) {
  data_model <- data_model %>%
    dm_add_fk(incidents,
              !!col,
              drivers,
              !!col)
}
dm_draw(
  data_model,
  view_type = "all",
  # Display all columns
  graph_attrs = "rankdir=RL",
  # Left-to-right layout
  node_attrs = "shape=rectangle",
  # Use rectangle-shaped nodes
  edge_attrs = "color=blue, penwidth=1",
  # Blue and bold edges for foreign keys
  column_types = TRUE,

)

}
