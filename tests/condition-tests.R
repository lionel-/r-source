
tryCatch2 <- function(expr, ...) {
    localCatch(...)
    expr
}
isSimpleError <- function(x, msg) {
    inherits(x, "simpleError") && identical(x$message, msg)
}
saveHandler <- function(name) {
    function(x) {
        assign(name, x, inherits = TRUE)
    }
}


## Short catch
err <- tryCatch2(stop("foo"), error = identity)
stopifnot(isSimpleError(err, "foo"))
## Long catch
f <- function() g()
g <- function() stop("foo")
err <- tryCatch2(f(), error = identity)
stopifnot(isSimpleError(err, "foo"))


## on.exit() handlers run after exiting condition handlers
cnd <- NULL
exit <- FALSE
f <- function(expr) {
    localCatch(condition = saveHandler("cnd"))
    on.exit(exit <<- cnd)
    expr
}
f(stop("foo"))
stopifnot(
    isSimpleError(cnd, "foo"),
    identical(exit, cnd)
)


## Rethrow
f <- function(expr) {
    localCatch(condition = function(...) stop("foo"))
    expr
}
err <- tryCatch(f(stop("bar")), error = identity)
stopifnot(isSimpleError(err, "foo"))


## Handlers are registered in reverse order
f <- function(expr) {
    localCatch(error = identity, condition = function(...) stop("foo"))
    expr
}
err <- f(stop("bar"))
stopifnot(isSimpleError(err, "bar"))
##
g <- function(expr) {
    localCatch(condition = function(...) stop("foo"), error = identity)
    expr
}
err <- g(stop("bar"))
stopifnot(isSimpleError(err, "foo"))


## on.exit() handlers are run after exiting handler
cnd <- simpleCondition("foo")
out <- NULL
exit <- NULL
f <- function() {
    on.exit(exit <<- out)
    tryCatch({ signalCondition(cnd); "ret" }, condition = function(x) out <<- x)
}
ret <- f()
stopifnot(
    identical(out, cnd),
    identical(exit, cnd),
    identical(ret, cnd)
)
##
out <- NULL
exit <- NULL
f <- function() {
    on.exit(exit <<- out)
    localCatch(condition = function(x) out <<- x)
    signalCondition(cnd)
    "ret"
}
ret <- f()
stopifnot(
    identical(out, cnd),
    identical(exit, cnd),
    identical(ret, cnd)
)

## return() in on.exit() overrides exiting handler value
f <- function() {
    on.exit(return("exit"))
    tryCatch(stop("foo"), error = function(...) "catch")
}
stopifnot(identical(f(), "exit"))
##
f <- function() {
    on.exit(return("exit"))
    localCatch(error = function(...) "catch")
    stop("foo")
}
stopifnot(identical(f(), "exit"))


## Handler stack is popped when on.exit() are called
cnd <- simpleError("foo")
f <- function() {
    on.exit(stop(cnd))
    tryCatch(stop("foo"), error = function(foo) "catch")
}
err <- tryCatch(f(), error = identity)
stopifnot(identical(err, cnd))
##
f <- function() {
    on.exit(stop(cnd))
    localCatch(error = function(foo) "catch")
    stop("foo")
}
err <- tryCatch(f(), error = identity)
stopifnot(identical(err, cnd))


## errbuf-based errors in body and on.exit expression. The on.exit
## error is caught and doesn't interrupt the jump from the earlier
## error. Reported by Bill Dunlap on R-devel (tryCatch in on.exit).
exit <- NULL
f <- function() {
    on.exit(
        tryCatch(expr = stop("exit"), error = function(x) exit <<- x)
    )
    stop("body")
}
out <- tryCatch(f(), error = identity)
stopifnot(
    identical(exit$message, "exit"),
    identical(out$message, "body")
)
## Things are a bit different if a local catcher is added from
## on.exit(). Conceptually, the handler belongs to f() and so the
## on.exit() error leaves the on.exit() context and effectively
## interrupts the current jump. It is still caught by the new handler
## on the stack.
exit <- NULL
f <- function() {
    on.exit({
        localCatch(error = function(x) (exit <<- x))
        stop("exit")
    })
    stop("body")
}
out <- tryCatch(f(), error = identity)
stopifnot(
    identical(exit$message, "exit"),
    identical(out$message, "exit")
)


## Local calling handler registered after local exiting handler
cnd <- simpleError("foo")
calling <- NULL
catch <- NULL
f <- function() {
    localCatch(error = function(x) (catch <<- x))
    localCallingHandlers(error = function(x) calling <<- x)
    stop(cnd)
}
ret <- f()
stopifnot(
    identical(calling, cnd),
    identical(catch, cnd),
    identical(ret, cnd)
)

## Local calling handler registered before local exiting handler
cnd <- simpleError("foo")
calling <- NULL
catch <- NULL
f <- function() {
    localCallingHandlers(error = function(x) calling <<- x)
    localCatch(error = function(x) (catch <<- x))
    stop(cnd)
}
ret <- f()
stopifnot(
    is.null(calling),
    identical(catch, cnd),
    identical(ret, cnd)
)


## Handlers must be supplied with class
f <- function() {
    list(
        tryCatch(error = identity, localCallingHandlers(identity)),
        tryCatch(error = identity, localCatch(identity))
    )
}
errs <- f()
stopifnot(
    grepl("must be supplied with a class", errs[[1]]$message),
    grepl("must be supplied with a class", errs[[2]]$message)
)

## Handler stack is reset when error occurs
f <- function() {
    tryCatch(error = identity, localCallingHandlers(foo = identity, identity))
    localCallingHandlers()
}
stopifnot(is.null(f()))


## Return current stack when called without arguments
f <- function() {
    stopifnot(
        is.null(localCallingHandlers()),
        is.null(localCatch())
    )
    localCallingHandlers(error = identity, warning = identity)
    localCatch(condition = identity, message = identity)
    list(
        calling = withVisible(localCallingHandlers()),
        exiting = withVisible(localCatch())
    )
}
stacks <- f()
stopifnot(
    isTRUE(stacks$calling$visible),
    isTRUE(stacks$exiting$visible),
    identical(stacks$calling$value, pairlist(error = identity, warning = identity)),
    identical(stacks$exiting$value, pairlist(condition = identity, message = identity))
)

## Return NULL invisibly when handlers are added
f <- function() {
    list(
        calling = withVisible(localCallingHandlers(condition = identity)),
        exiting = withVisible(localCatch(condition = identity))
    )
}
stacks <- f()
stopifnot(
    isFALSE(stacks$calling$visible),
    isFALSE(stacks$exiting$visible),
    is.null(stacks$calling$value),
    is.null(stacks$exiting$value)
)

## All active handlers are returned
f <- function() {
    localCallingHandlers(foo = identity)
    g()
}
g <- function() {
    localCallingHandlers(bar = identity)
    h()
}
h <- function() {
    localCallingHandlers()
}
stopifnot(identical(f(), pairlist(bar = identity, foo = identity)))


## Handler stacks in intervening contexts are updated
f <- function() {
    list(intermediate = g(environment()), target = localCallingHandlers())
}
g <- function(env) {
    h(env)
    localCallingHandlers()
}
h <- function(env) {
    localCallingHandlers(foo = identity, .envir = env)
}
stacks <- f()
stopifnot(
    identical(stacks$intermediate, pairlist(foo = identity)),
    identical(stacks$target, pairlist(foo = identity))
)
## Stack has changed on the way
f <- function() {
    list(intermediate = g(environment()), target = localCallingHandlers())
}
g <- function(env) {
    h(env)
    localCallingHandlers()
}
h <- function(env) {
    # Change stack on the way
    localCallingHandlers(baz = identity)
    i(env)
}
i <- function(env) {
    localCallingHandlers(foo = identity, .envir = env)
}
stacks <- f()
stopifnot(
    identical(stacks$intermediate, pairlist(foo = identity)),
    identical(stacks$target, pairlist(foo = identity))
)


## Supplying NULL removes global handlers
globalCallingHandlers(foo = NULL)
stopifnot(is.null(globalCallingHandlers()))
##
globalCallingHandlers(foo = identity, bar = identity, foo = identity)
globalCallingHandlers(foo = NULL)
stopifnot(identical(globalCallingHandlers(), pairlist(bar = identity)))
globalCallingHandlers(bar = NULL)
stopifnot(is.null(globalCallingHandlers()))
## `condition = NULL` removes all global handlers
globalCallingHandlers(foo = identity, bar = identity, foo = identity)
globalCallingHandlers(condition = NULL)
stopifnot(is.null(globalCallingHandlers()))


## Can't remove local calling handlers
f <- function() {
    localCallingHandlers(foo = identity)
    localCallingHandlers(foo = NULL)
}
stack <- NULL
err <- tryCatch(
    withCallingHandlers(
        f(),
        error = function(...) stack  <<- localCallingHandlers()
    ),
    error = identity
)
stopifnot(grepl("inside functions", err$message))


## Removing global handlers preserves local ones
##
globalHandler <- function(...) "global"
localHandler <- function(...) "local"
f <- function() g()
##
## Remove all globals
globalCallingHandlers(foo = globalHandler, bar = identity)
g <- function() {
    localCallingHandlers(foo = localHandler)
    globalCallingHandlers(condition = NULL)
    localCallingHandlers()
}
stack <- f()
stopifnot(
    identical(stack, pairlist(foo = localHandler)),
    is.null(globalCallingHandlers())
)
##
## Remove global handlers on the top
globalCallingHandlers(foo = globalHandler, bar = identity)
g <- function() {
    localCallingHandlers(foo = localHandler)
    globalCallingHandlers(foo = NULL)
    localCallingHandlers()
}
stack <- f()
stopifnot(
    identical(stack, pairlist(foo = localHandler, bar = identity)),
    identical(globalCallingHandlers(), pairlist(bar = identity))
)
globalCallingHandlers(condition = NULL)
##
## Remove global handlers on the bottom
globalCallingHandlers(bar = identity, foo = globalHandler)
g <- function() {
    localCallingHandlers(foo = localHandler)
    globalCallingHandlers(foo = NULL)
    localCallingHandlers()
}
stack <- f()
stopifnot(
    identical(stack, pairlist(foo = localHandler, bar = identity)),
    identical(globalCallingHandlers(), pairlist(bar = identity))
)
globalCallingHandlers(condition = NULL)

## `globalCallingHandlers()` doesn't return local handlers
globalCallingHandlers(foo = identity)
f <- function() {
    localCallingHandlers(bar = identity)
    list(local = localCallingHandlers(), global = globalCallingHandlers())
}
stack <- f()
stopifnot(
    identical(stack$local, pairlist(bar = identity, foo = identity)),
    identical(stack$global, pairlist(foo = identity))
)
globalCallingHandlers(condition = NULL)


## Can resignal conditions from global handlers
resignalled <<- NULL
muffled <<- NULL
resignal <- function(wrn) {
    new <- structure(
        list(message = paste0("Caught: ", conditionMessage(wrn))),
        class = c("mywarning", "warning", "condition")
    )
    resignalled <<- new
    warning(new)
    invokeRestart("muffleWarning")
}
muffle <- function(wrn) {
    muffled <<- wrn
    invokeRestart("muffleWarning")
}
globalCallingHandlers(simpleWarning = resignal, warning = muffle)
warning("foo")
stopifnot(
    inherits(resignalled, "mywarning"),
    identical(resignalled, muffled)
)
globalCallingHandlers(condition = NULL)
