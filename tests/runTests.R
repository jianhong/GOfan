require("GOfan") || stop("unable to load Package:GOfan")
require("testthat") || stop("unable to load testthat")
require("org.Dr.eg.db") || stop("unable to load org.Dr.eg.db")
require("igraph") || stop("unable to load igraph")
require("AnnotationDbi") || stop("unable to load AnnotationDbi")


test_check("GOfan")
