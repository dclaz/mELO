################################################################################
# Utility Functions
################################################################################

# Print a message with timestamp to screen
print_time_status <- function(message){
    cat(
        paste0(
            Sys.time(), "\t",
            message, "\n"
        )
    )
}
