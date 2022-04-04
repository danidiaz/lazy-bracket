# lazy-bracket

Sometimes, when `bracket`ing some piece of code, the
resource acquired by the `bracket` won't be actually used:

- Finding a result in an in-memory cache can mean that a database query is
avoided, and the database connection stays untouched. 

- You might be providing some resource (again, think database connection) to every
REST endpoint handler in your API, even if some handlers don't make use of the
resource, because treating these handlers as special cases would be tedious.

In order to be more frugal and avoid unnecessary resource acquisitions, one
possible approach is to delay the acquisition to 
 the first time the resource
is actually used, if it's used at all. 

What's more, certain "control" operations
on the resource don't need to be applied immediately, and instead can wait
until the resource is eventually acquired, or be omitted altogether if the resource isn't acquired:

- It's wasteful to acquire a file handle just to perform `hSetBuffering`, if we are not going to write to the handle.

- It's wasteful to acquire a database connection just to begin a transaction, if we aren't going to perform any query.

This package provides lazy versions of `bracket` for which resource acquisition
is delayed until first use, and control operations are only applied once resources are acquired.

## Links

- The inspiration:
[LazyConnectionDataSourceProxy](https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/jdbc/datasource/LazyConnectionDataSourceProxy.html)
form Java's Spring Framework.

  > Proxy for a target DataSource, fetching actual JDBC Connections lazily, i.e. not until first creation of a Statement. Connection initialization properties like auto-commit mode, transaction isolation and read-only mode will be kept and applied to the actual JDBC Connection as soon as an actual Connection is fetched (if ever). Consequently, commit and rollback calls will be ignored if no Statements have been created.

  > This DataSource proxy allows to avoid fetching JDBC Connections from a pool unless actually necessary. JDBC transaction control can happen without fetching a Connection from the pool or communicating with the database; this will be done lazily on first creation of a JDBC Statement.