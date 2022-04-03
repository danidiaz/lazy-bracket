# lazy-bracket

## Links

- [LazyConnectionDataSourceProxy](https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/jdbc/datasource/LazyConnectionDataSourceProxy.html) form Java's Spring Framework.

  > Proxy for a target DataSource, fetching actual JDBC Connections lazily, i.e. not until first creation of a Statement. Connection initialization properties like auto-commit mode, transaction isolation and read-only mode will be kept and applied to the actual JDBC Connection as soon as an actual Connection is fetched (if ever). Consequently, commit and rollback calls will be ignored if no Statements have been created.

  > This DataSource proxy allows to avoid fetching JDBC Connections from a pool unless actually necessary. JDBC transaction control can happen without fetching a Connection from the pool or communicating with the database; this will be done lazily on first creation of a JDBC Statement.