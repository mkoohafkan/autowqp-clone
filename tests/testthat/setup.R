skip_if_no_db = function(db) {
  if (!wqpr::wqp_check_connection(db)) {
    skip("WQP connection not available")
  }
}
