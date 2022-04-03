# lazy-bracket

Sometimes, when `bracket`ing some piece of code, the acquired resource won't be
actually used:

- Finding a result in an in-memory cache can mean that a database query is
avoided, and that the database connection stays untouched. 

- You might be providing some resource (say, a database connection) to every
REST endpoint handler in your API, even if some handlers don't make use of the
resource. And yet, treating these endpoints as special cases would be tedious.

In order to be more frugal and avoid unnecessary resource acquisitions, one
possible approach is to delay the acquisition to the last possible moment, to
the first time the resource is actually used. Even better: certain operations
on the resource don't need to be applied immediately, and instead can wait
until the resource is eventually acquired.

This package implements that approach.

## Links

- The inspiration:
[LazyConnectionDataSourceProxy](https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/jdbc/datasource/LazyConnectionDataSourceProxy.html)
form Java's Spring Framework.

  > Proxy for a target DataSource, fetching actual JDBC Connections lazily, i.e. not until first creation of a Statement. Connection initialization properties like auto-commit mode, transaction isolation and read-only mode will be kept and applied to the actual JDBC Connection as soon as an actual Connection is fetched (if ever). Consequently, commit and rollback calls will be ignored if no Statements have been created.

  > This DataSource proxy allows to avoid fetching JDBC Connections from a pool unless actually necessary. JDBC transaction control can happen without fetching a Connection from the pool or communicating with the database; this will be done lazily on first creation of a JDBC Statement.