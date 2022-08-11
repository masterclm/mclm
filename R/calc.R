# Moved from mclm.R
# -- All calc.X() functions below in principle have become
# -- redundant, because their functionality has become subsumed by
# -- assoc_scores_abcd();
# -- they are kept nonetheless, because:
#      (i)  I'm still keeping open the option to (redundantly) also
#           export these functions (next to assoc_scores_abcd(),
#           simply because their source code is easier to grasp
#           for readers of MCLM
#      (ii) they can still be useful for debugging assoc_scores_abcd()
# -- However, for the moment, these functions are not exported.

calc.exp.a <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn
  return(ea)
}

calc.min.exp <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  return(apply(cbind(ea,eb,ec,ed), 1, min))
}

calc.G2 <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  return(2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
}

calc.G2.is.pos <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d
  return(a/m > c/n)
}

calc.p.G2 <- function(a,b,c,d) {
  g2 <- calc.G2(a,b,c,d)
  return(1 - pchisq(g2, 1))
}

calc.G2.pos <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- (2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
  result[!(a/m > c/n)] <- 0.0
  return(result)
}

calc.p.G2.pos <- function(a,b,c,d) {
  g2 <- calc.G2.pos(a,b,c,d)
  return(1 - pchisq(g2, 1))
}

calc.G2.neg <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- (2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
  result[(a/m > c/n)] <- 0.0
  return(result)
}

calc.p.G2.neg <- function(a,b,c,d) {
  g2 <- calc.G2.neg(a,b,c,d)
  return(1 - pchisq(g2, 1))
}

calc.G2.signed <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- (2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
  result[!(a/m > c/n)] <- - result[!(a/m > c/n)]
  return(result)
}

calc.Chi2 <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  return((a - ea)^2/ea + (b - eb)^2/eb + (c - ec)^2/ec + (d - ed)^2/ed)
}

calc.Chi2.signed <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- ((a - ea)^2/ea + (b - eb)^2/eb + (c - ec)^2/ec + (d - ed)^2/ed)
  result[!(a/m > c/n)] <- - result[!(a/m > c/n)]
  return(result)  
}
calc.p.Chi2 <- function(a,b,c,d) {
  chi2 <- calc.Chi2(a,b,c,d)
  return(1 - pchisq(chi2, 1))
}

calc.PMI <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  return(log2( (a/mn) / ( (k/mn) * (m/mn) ) ))
}

calc.MS <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  return(min(a/m, a/k))
}

calc.t <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  t <- ( ( a/mn - k/mn * m/mn )   /
           sqrt( ( (a/mn) * (1 - a/mn) ) / mn ) )
  return(t)
}

calc.p.t <- function(a,b,c,d) { # 1-sided
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  t <- ( ( a/mn - k/mn * m/mn )   /
           sqrt( ( (a/mn) * (1 - a/mn) ) / mn ) )
  return(1 - pt(t, mn-1))
}

calc.p.t.two.sided <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  t <- ( ( a/mn - k/mn * m/mn )   /
           sqrt( ( (a/mn) * (1 - a/mn) ) / mn ) )
  p <- 1 - pt(t, mn-1) # first assumption: positive deviation
  if (t < 0) {
    p <- pt(t, mn-1)   # correct previous assumption if needed
  }
  p <- 2*p             # two.sided
  return(p)
}


calc.DP <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d
  return((a/m) - (c/n))
}

calc.RR <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d
  return((a/m) / (c/n))
}

calc.OR <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  return((a/b) / (c/d))
}

calc.log.OR <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  return(log((a/b) / (c/d)))
}

calc.DICE <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; k <- a + c
  return( (2*a)/(m+k) )
}

calc.p.fisher <- function(a,b,c,d) {
  # the use of zero_plus() doesn't seem to hinder, so we keep it
  # for the sake of conceptual consistency across the
  # calc.X() functions
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c
  return (1 - phyper(a-1,m,n,k))
}

calc.p.fisher.two.sided <- function(a,b,c,d) {
  # no zero_plus() here, to avoid warning about non-integer values
  result <- vector()
  for (i in 1:length(a)) {
    m <- matrix(c(a[i],b[i],c[i],d[i]), nrow=2, byrow=T)
    result[i] <- fisher.test(m)$p
  }
  return(result)
}

