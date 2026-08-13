fixture_metadata <- function() {
  list(
    schemas_description = data.table::data.table(
      schema = c("public", "audit"),
      description = c("[EN] Public data\n\n[FR] Données publiques", NA_character_)
    ),
    tables_description = data.table::data.table(
      schema = c("public", "public", "public", "audit"),
      table = c("account", "membership", "standalone", "event"),
      description = c("Accounts", "Memberships", NA_character_, "Events")
    ),
    tables_columns = data.table::data.table(
      schema = c(rep("public", 8), rep("audit", 3)),
      table = c(rep("account", 3), rep("membership", 4), "standalone", rep("event", 3)),
      column = c("tenant_id", "account_id", "name", "tenant_id", "account_id", "group_id", "note", "id", "id", "tenant_id", "account_id"),
      type = c(rep("integer", 2), "text", rep("integer", 3), "text", rep("integer", 4)),
      mandatory = c(rep("YES", 6), "NO", "YES", "YES", "YES", "NO"),
      description = c("Tenant", "Account", "Name", "Tenant", "Account", "Group", NA, NA, "Event", "Tenant", "Account")
    ),
    tables_foreign_keys = data.table::data.table(
      schema = c("public", "audit"), table = c("membership", "event"),
      foreign_key = c("membership_account_fk", "event_account_fk"),
      columns = list(c("tenant_id", "account_id"), c("tenant_id", "account_id")),
      target_schema = c("public", "public"), target_table = c("account", "account"),
      target_columns = list(c("tenant_id", "account_id"), c("tenant_id", "account_id"))
    ),
    tables_primary_keys = data.table::data.table(
      schema = c("public", "public", "public", "audit"),
      table = c("account", "membership", "standalone", "event"),
      primary_key_columns = list(c("tenant_id", "account_id"), c("tenant_id", "account_id", "group_id"), "id", "id")
    )
  )
}
