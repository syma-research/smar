test_parallel <- function(nproc, time = 2) {
  system.time(
    future.apply::future_sapply(
      seq_len(nproc),
      function(iproc)
        Sys.sleep(time)
    )
  )
}
