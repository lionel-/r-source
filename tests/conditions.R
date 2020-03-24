
# TODO: Add to base package
globalConditionHandlers <- function(...) {
    handlers <- list(...)
    if (identical(handlers, list(NULL))) {
	.Internal(.resetGlobHands())
    } else {
	# Unwrap list of handlers
	if (length(handlers) == 1 && is.list(handlers[[1]]))
	    handlers <- handlers[[1]]
	classes <- names(handlers)
	if (length(classes) != length(handlers))
	    stop("Handlers must be specified with a condition class.")
	.Internal(.addGlobHands(classes, handlers, .GlobalEnv, NULL, TRUE))
    }
}

saveTo <- function(to) function(cond) assign(to, cond, envir = globalenv())
isCond <- function(x, class, msg) {
    inherits(x, "condition") && inherits(x, class) && identical(x$message, msg)
}

## Handler stack starts empty
handlers <- withVisible(globalConditionHandlers())
stopifnot(
    identical(handlers$value, list()),
    isTRUE(handlers$visible)
)

## Can register and inspect handler
globalConditionHandlers(error = saveTo(".Last.error"))
handlers <- withVisible(globalConditionHandlers())

stopifnot(
    length(handlers$value) == 1,
    names(handlers$value) == "error",
    handlers$visible
)

## Handlers are invoked based on class
.Last.error <- NULL
signalCondition(simpleCondition("foo"))
stopifnot(is.null(.Last.error))
signalCondition(simpleError("foo"))
stopifnot(isCond(.Last.error, "error", "foo"))

## Can register multiple handlers
globalConditionHandlers(
    condition = saveTo(".Last.condition"),
    warning = saveTo(".Last.warning")
)
handlers <- globalConditionHandlers()
stopifnot(length(handlers) == 3, all(names(handlers) == c("condition", "warning", "error")))

## Multiple handlers are invoked
.Last.error <- NULL
signalCondition(simpleWarning("bar"))
stopifnot(
    is.null(.Last.error),
    isCond(.Last.condition, "warning", "bar"),
    isCond(.Last.warning, "warning", "bar")
)

## Handlers are not invoked if error is caught
.Last.error <- NULL
try(stop("baz"), TRUE)
stopifnot(is.null(.Last.error))

## Can remove handlers
handlers <- globalConditionHandlers()
old <- withVisible(globalConditionHandlers(NULL))
stopifnot(identical(old$value, handlers), !old$visible)

## Can pass list of handlers
foobars <- list(foo = function() "foo", bar = function() "bar")
globalConditionHandlers(foobars)
stopifnot(identical(globalConditionHandlers(), foobars))
globalConditionHandlers(NULL)

## Local handlers are not returned
handlers <- withCallingHandlers(foo = function(...) NULL, globalConditionHandlers())
stopifnot(identical(handlers, list()))
globalConditionHandlers(foobars)
handlers <- withCallingHandlers(foo = function(...) NULL, globalConditionHandlers())
stopifnot(identical(handlers, foobars))
globalConditionHandlers(NULL)
