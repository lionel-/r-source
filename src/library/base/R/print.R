#  File src/library/base/R/print.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

print <- function(x, ...) {
    if (.Internal(useCustomAutoprint(parent.frame()))) {
	# Prevent recursion into autoprint
	old <- options(.customAutoprintOngoing = sys.nframe())
	on.exit(options(old))

	custom <- getOption("autoprint")
	custom(x, ...)

	invisible(x)
    } else {
	UseMethod("print")
    }
}

##- Need '...' such that it can be called as  NextMethod("print", ...):
print.default <- function(x, digits = NULL, quote = TRUE, na.print = NULL,
                          print.gap = NULL, right = FALSE, max = NULL,
			  useSource = TRUE, indexTag = "", ...)
{
    # Arguments are wrapped in another pairlist because we need to
    # forward them to recursive print() calls.
    args <- pairlist(
	digits = digits,
	quote = quote,
	na.print = na.print,
	print.gap = print.gap,
	right = right,
	max = max,
	useSource = useSource,
        ...
    )

    # Missing elements are not forwarded so we pass their
    # `missingness`. Also this helps decide whether to call show()
    # with S4 objects (if any argument print() is used instead).
    missings <- c(missing(digits), missing(quote), missing(na.print),
		  missing(print.gap), missing(right), missing(max),
		  missing(useSource))

    isString <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
    stopifnot(isString(indexTag))

    caller <- parent.frame()

    # If `print()` is directly called back from the customisation
    # function, and the default method is selected, this means that
    # the custom function should be used again when printing elements
    # inside of `x` (if it is a list, or via attributes). Normally, we
    # inspect the caller environment and check whether it inherits
    # from the global environment to determine whether to use the
    # autoprint function. This is problematic if the customisation
    # function is exported from a package, as it might not inherit
    # from the global environment. We fix this here. The tricky part
    # is that we must not force customisation if we're being called
    # back from another print method. Inspecting the call stack from
    # the point where custom autoprint was invoked seems the most
    # robust course of action. If we're falling back from the custom
    # function, the current frame should be adjascent to the first
    # print() frame on the stack. Otherwise, it means we're being
    # called from another print method.
    i <- getOption(".customAutoprintOngoing")
    if (is.integer(i)) {
	i <- i + 1L
	top <- sys.nframe()
	while (i < top) {
	    if (identical(sys.function(i), print)) {
		if (i + 1L == top)
		    caller <- globalenv()
		break
	    }
	    i <- i + 1L
	    next
	}
    }

    # Use autoprint customisation if we're called from the global env.
    # Except if we're called from a print method via NextMethod(), as
    # the caller is artificially forwarded in this case.
    useCustom <-
	identical(topenv(caller), globalenv()) &&
	(!exists(".Class") || is.null(attr(.Class, "previous")))

    # Reset custom autoprinting flag so we can recurse into
    # getOption("autoprint") when printing lists. This relies on
    # do_printdefault() _not_ calling PrintDispatch(), which would
    # cause infinite recursion. Instead it calls PrintValueRec() which
    # ensures we'll only dispatch on elements.
    old <- options(.customAutoprintOngoing = NULL)
    on.exit(options(old))

    .Internal(print.default(x, caller, useCustom, indexTag, args, missings))
}

prmatrix <-
    function (x, rowlab = dn[[1]], collab = dn[[2]],
              quote = TRUE, right = FALSE,
              na.print = NULL, ...)
{
    x <- as.matrix(x)
    dn <- dimnames(x)
    .Internal(prmatrix(x, rowlab, collab, quote, right, na.print))
}

noquote <- function(obj, right = FALSE) {
    ## constructor for a useful "minor" class
    if(!inherits(obj,"noquote"))
        class(obj) <- c(attr(obj, "class"),
                        if(right) c(right = "noquote") else "noquote")
    obj
}

as.matrix.noquote <- function(x, ...) noquote(NextMethod("as.matrix", x))

as.data.frame.noquote <- as.data.frame.vector

c.noquote <- function(..., recursive = FALSE)
    structure(NextMethod("c"), class = "noquote")

`[.noquote` <- function (x, ...) {
    attr <- attributes(x)
    r <- unclass(x)[...] ## shouldn't this be NextMethod?
    attributes(r) <- c(attributes(r),
		       attr[is.na(match(names(attr),
                                        c("dim","dimnames","names")))])
    r
}

print.noquote <- function(x, quote = FALSE, right = FALSE, ...) {
    if(copy <- !is.null(cl <- attr(x, "class"))) {
	isNQ <- cl == "noquote"
	if(missing(right))
	    right <- any("right" == names(cl[isNQ]))
	if(copy <- any(isNQ)) {
	    ox <- x
	    cl <- cl[!isNQ]
	    attr(x, "class") <- if(length(cl)) cl # else NULL
	}
    }
    print(x, quote = quote, right = right, ...)
    invisible(if(copy) ox else x)
}

## for alias.lm, aov
print.listof <- function(x, ...)
{
    nn <- names(x)
    ll <- length(x)
    if(length(nn) != ll) nn <- paste("Component", seq.int(ll))
    for(i in seq_len(ll)) {
	cat(nn[i], ":\n"); print(x[[i]], ...); cat("\n")
    }
    invisible(x)
}

## formerly same as [.AsIs
`[.listof` <- function(x, i, ...) structure(NextMethod("["), class = class(x))
`[.Dlist` <- `[.simple.list` <- `[.listof`

## used for version:
print.simple.list <- function(x, ...)
    print(noquote(cbind("_"=unlist(x))), ...)

print.function <- function(x, useSource = TRUE, ...)
    print.default(x, useSource=useSource, ...)

## used for getenv()
print.Dlist <- function(x, ..., style = c("table", "list"),
			width = 0.9 * getOption("width"),
			indent = NULL)
{
    if(!is.list(x) && !is.matrix(x) && is.null(names(x))) ## messed up Dlist
	return(NextMethod())
    cat(formatDL(x, style = style, width = width, indent = indent), sep="\n")
    invisible(x)
}
