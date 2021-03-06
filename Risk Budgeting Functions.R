# normalVaR.r
##
## Normal VaR functions for portfolio VaR report
##

normalVaR <- function(mu, sigma, tail.prob = 0.01, invert=FALSE) {
    ## compute normal VaR for collection of assets given mean and sd vector
    ## inputs:
    ## mu         n x 1 vector of expected returns
    ## sigma      n x 1 vector of standard deviations
    ## tail.prob  scalar tail probability
    ## invert     logical. If TRUE report VaR as positive number
    ## output:
    ## VaR        n x 1 vector of left tail return quantiles
    ## References:
    ## Jorian (2007) pg 111.
    mu = as.matrix(mu)
    sigma = as.matrix(sigma)
    if ( nrow(mu) != nrow(sigma) )
        stop("mu and sigma must have same number of elements")
    if ( tail.prob < 0 || tail.prob > 1)
        stop("tail.prob must be between 0 and 1")
    VaR = mu + sigma*qnorm(tail.prob)
    if (invert) {
        VaR = -VaR
    }
    return(VaR)
}

# normalES.r
normalES <- function(mu, sigma, tail.prob = 0.01, invert=FALSE) {
    ## compute normal ES for collection of assets given mean and sd vector
    ## inputs:
    ## mu       n x 1 vector of expected returns
    ## sigma    n x 1 vector of standard deviations
    ## tail.prob  scalar tail probability
    ## invert   logical. If TRUE, return ES as positive number
    ## output:
    ## ES      n x 1 vector of left tail average returns reported as a positive number
    mu = as.matrix(mu)
    sigma = as.matrix(sigma)
    if ( nrow(mu) != nrow(sigma) )
        stop("mu and sigma must have same number of elements")
    if ( tail.prob < 0 || tail.prob > 1)
        stop("tail.prob must be between 0 and 1")
    ES = mu - sigma*dnorm(qnorm(tail.prob))/tail.prob
    if(invert) {
        ES = -ES
    }
    return(ES)
}

## factorModelFactorRiskDecomposition.r
## 
## purpose: Compute factor model factor risk (sd) decomposition for individual 
##          fund
## author: Eric Zivot
## created: August 13, 2009
## revision history: 
## July 1, 2010
##    Added comment to inputs
## June 8, 2010
##    Added percent contribution to risk as output

factorModelFactorSdDecomposition <- function(beta.vec, factor.cov, sig2.e) {
    ## Inputs:
    ## beta       	   k x 1 vector of factor betas with factor names in the rownames
    ## factor.cov		 k x k factor excess return covariance matrix
    ## sig2.e			   scalar, residual variance from factor model
    ## Output:
    ## A list with the following components:
    ## sd.fm              scalar, std dev based on factor model
    ## mcr.fm             k+1 x 1 vector of factor marginal contributions to risk (sd)
    ## cr.fm              k+1 x 1 vector of factor component contributions to risk (sd)
    ## pcr.fm             k+1 x 1 vector of factor percent contributions to risk (sd)
    ## Remarks:
    ## The factor model has the form
    ## R(t) = beta'F(t) + e(t) = beta.star'F.star(t)
    ## where beta.star = (beta, sig.e)' and F.star(t) = (F(t)', z(t))'
    ## By Euler's theorem
    ## sd.fm = sum(cr.fm) = sum(beta*mcr.fm)
    if(is.matrix(beta.vec)) {
        beta.names = c(rownames(beta.vec), "residual")
    } else if(is.vector(beta.vec)) {
        beta.names = c(names(beta.vec), "residual")
    } else {
        stop("beta.vec is not a matrix or a vector")
    }  
    beta.vec = as.vector(beta.vec)
    beta.star.vec = c(beta.vec, sqrt(sig2.e))
    names(beta.star.vec) = beta.names
    factor.cov = as.matrix(factor.cov)
    k.star = length(beta.star.vec)
    k = k.star - 1
    factor.star.cov = diag(k.star)
    factor.star.cov[1:k, 1:k] = factor.cov
    
    ## compute factor model sd
    sd.fm = as.numeric(sqrt(t(beta.star.vec) %*% factor.star.cov %*% beta.star.vec))
    ## compute marginal and component contributions to sd
    mcr.fm = (factor.star.cov %*% beta.star.vec)/sd.fm
    cr.fm = mcr.fm * beta.star.vec
    pcr.fm = cr.fm/sd.fm
    rownames(mcr.fm) <- rownames(cr.fm) <- rownames(pcr.fm) <- beta.names
    colnames(mcr.fm) = "MCR"
    colnames(cr.fm) = "CR"
    colnames(pcr.fm) = "PCR"
    ## return results
    ans = list(sd.fm = sd.fm,
               mcr.fm = t(mcr.fm),
               cr.fm = t(cr.fm),
               pcr.fm = t(pcr.fm))
    return(ans)
}

## factorModelFactorEsDecomposition.r
##
## author: Eric Zivot
## created: January 1, 2009
## revised: March 11, 2011
factorModelFactorEsDecomposition <- function(bootData, beta.vec, sig2.e, tail.prob = 0.01,
                                             method=c("average"),
                                             VaR.method=c("HS", "CornishFisher")) {
    ## Compute factor model factor ES decomposition based on Euler's theorem given historic 
    ## or simulated data and factor model parameters.
    ## The partial derivative of ES wrt factor beta is computed
    ## as the expected factor return given fund return is less than or equal to its VaR
    ## VaR is compute either as the sample quantile or as an estimated quantile
    ## using the Cornish-Fisher expansion
    ## inputs:
    ## bootData   B x (k+2) matrix of bootstrap data. First column contains the fund returns,
    ##            second through k+1 columns contain factor returns, (k+2)nd column contain residuals
    ##            scaled to have variance 1.
    ## beta.vec   k x 1 vector of factor betas
    ## sig2.e    		scalar, residual variance from factor model
    ## tail.prob  scalar tail probability
    ## method     character, method for computing marginal ES. Valid choices are
    ##            "average" for approximating E[Fj | R<=VaR]
    ## VaR.method character, method for computing VaR. Valid choices are "HS" for
    ##            historical simulation (empirical quantile); "CornishFisher" for
    ##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
    ##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
    ##            package
    ## output:
    ## A list with the following components:
    ## VaR.fm              scalar, bootstrap VaR value for fund reported as a positive number
    ## n.exceed            scalar, number of observations beyond VaR
    ## idx.exceed          n.exceed x 1 vector giving index values of exceedences
    ## ES.fm               scalar, bootstrap ES value for fund reported as a positive number
    ## mcES.fm             k+1 x 1 vector of factor marginal contributions to ES
    ## cES.fm              k+1 x 1 vector of factor component contributions to ES
    ## pcES.fm             k+1 x 1 vector of factor percent contributions to ES
    ## Remarks:
    ## The factor model has the form
    ## R(t) = beta'F(t) + e(t) = beta.star'F.star(t)
    ## where beta.star = (beta, sig.e)' and F.star(t) = (F(t)', z(t))'
    ## By Euler's theorem
    ## ES.fm = sum(cES.fm) = sum(beta.star*mcES.fm)
    ## References:
    ## 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
    ##    The Journal of Risk 5/2.
    ## 2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and
    ##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
    ##    Bank of Japan.
    ## 3. Meucci (2007). "Risk Contributions from Generic User-Defined Factors," Risk.
    require(PerformanceAnalytics)
    VaR.method = VaR.method[1]
    bootData = as.matrix(bootData)
    ncol.bootData = ncol(bootData)
    if(is.matrix(beta.vec)) {
        beta.names = c(rownames(beta.vec), "residual")
    } else if(is.vector(beta.vec)) {
        beta.names = c(names(beta.vec), "residual")
    } else {
        stop("beta.vec is not an n x 1 matrix or a vector")
    }  
    #  beta.names = c(names(beta.vec), "residual")
    beta.star.vec = c(beta.vec, sqrt(sig2.e))
    names(beta.star.vec) = beta.names
    
    if (VaR.method == "HS") {
        VaR.fm = quantile(bootData[, 1], prob=tail.prob)
        idx = which(bootData[, 1] <= VaR.fm)
        ES.fm = -mean(bootData[idx, 1])
    } else {
        VaR.fm = -VaR.CornishFisher(bootData[, 1], p=(1-tail.prob))
        idx = which(bootData[, 1] <= pVaR)
        ES.fm = -mean(bootData[idx, 1])
    }
    ##
    ## compute marginal contribution to ES
    ##
    if (method == "average") {
        ## compute marginal ES as expected value of factor return given fund
        ## return is less than or equal to VaR
        mcES.fm = -as.matrix(colMeans(bootData[idx, -1]))
    } else {
        stop("invalid method")
    }
    
    ## compute correction factor so that sum of weighted marginal ES adds to portfolio ES
    #cf = as.numeric( ES.fm / sum(mcES.fm*beta.star.vec) )
    #mcES.fm = cf*mcES.fm
    cES.fm = mcES.fm*beta.star.vec
    pcES.fm = cES.fm/ES.fm
    colnames(mcES.fm) = "MCES"
    colnames(cES.fm) = "CES"
    colnames(pcES.fm) = "PCES"
    ans = list(VaR.fm = -VaR.fm,
               n.exceed = length(idx),
               idx.exceed = idx,
               ES.fm = ES.fm, 
               mcES.fm = t(mcES.fm), 
               cES.fm = t(cES.fm),
               pcES.fm = t(pcES.fm))
    return(ans)
}

## portfolioSdDecomposition.r
##
## purpose: Compute portfolio sd (risk) decomposition by asset
## author: Eric Zivot
## created: March 11, 2011
## revised: March 11, 2011
## references:
## Qian, Hua and Sorensen (2007) Quantitative Equity Portfolio Management, 
## chapter 3.

portfolioSdDecomposition <- function(w.vec, cov.assets) {
    ## Inputs:
    ## w.vec         n x 1 vector of portfolio weights
    ## cov.assets    	 n x n asset covariance matrix
    ## Output:
    ## A list with the following components:
    ## sd.p          scalar, portfolio sd
    ## mcsd.p        1 x n vector, marginal contributions to portfolio sd
    ## csd.p         1 x n vector, contributions to portfolio sd
    ## pcsd.p        1 x n vector, percent contribution to portfolio sd
    
    if (any(diag(chol(cov.assets)) == 0))
        warning("Asset covariance matrix is not positive definite")
    ## compute portfolio level variance
    var.p = as.numeric(t(w.vec) %*% cov.assets %*% w.vec)
    sd.p = sqrt(var.p)
    ## compute marginal, component and percentage contributions to risk
    mcsd.p = (cov.assets %*% w.vec)/sd.p
    csd.p = w.vec*mcsd.p
    pcsd.p = csd.p/sd.p
    colnames(mcsd.p) = "MCSD"
    colnames(csd.p) = "CSD"
    colnames(pcsd.p) = "PCSD"
    ## return results
    ans = list(sd.p=sd.p,
               mcsd.p=t(mcsd.p),
               csd.p=t(csd.p),
               pcsd.p=t(pcsd.p))
    return(ans)
}

## portfolioEsDecomposition.r
## purpose: Compute portfolio ES (risk) decomposition by asset
## author: Eric Zivot
## created: March 11, 2011
## revised: March 11, 2011
##
## note: this functionality is provided by the ES() function in PerformanceAnalytics

portfolioEsDecomposition <- function(bootData, w, delta.w = 0.001, tail.prob = 0.01,
                                     method=c("derivative", "average"),
                                     VaR.method=c("HS", "CornishFisher")) {
    ## Compute portfolio ES decomposition given historical or simulated data and portfolio weights.
    ## Marginal ES is computed either as the numerical derivative of ES wrt portfolio weight or
    ## as the expected fund return given portfolio return is less than or equal to portfolio VaR
    ## VaR is compute either as the sample quantile or as an estimated quantile
    ## using the Cornish-Fisher expansion
    ## inputs:
    ## bootData   B x n matrix of B bootstrap returns on assets in portfolio.
    ## w          n x 1 vector of portfolio weights
    ## delta.w    scalar, change in portfolio weight for computing numerical derivative
    ## tail.prob  scalar tail probability
    ## method     character, method for computing marginal ES. Valid choices are
    ##            "derivative" for numerical computation of the derivative of portfolio
    ##            ES wrt fund portfolio weight; "average" for approximating E[Ri | Rp<=VaR]
    ## VaR.method character, method for computing VaR. Valid choices are "HS" for
    ##            historical simulation (empirical quantile); "CornishFisher" for
    ##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
    ##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
    ##            package
    ## output:
    ## pVaR       scalar, portfolio VaR reported as a positive number
    ## pES        scalar, portfolio ES reported as a positive number
    ## n.exceed   scalar, number of VaR exceedences
    ## idx.exceed n.exceed x 1 vector of exceedence indices
    ## mES        1 x n matrix of marginal ES values for each fund
    ## cES        1 x n matrix of component ES values
    ## pcES       1 x n matrix of percent contributions to portfolio ES values
    ## References:
    ## 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
    ##    The Journal of Risk 5/2.
    ## 2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and
    ##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
    ##    Bank of Japan.
    require(PerformanceAnalytics)
    method = method[1]
    VaR.method = VaR.method[1]
    bootData = as.matrix(bootData)
    w = as.matrix(w)
    if ( ncol(bootData) != nrow(w) )
        stop("Columns of bootData and rows of w do not match")
    if ( tail.prob < 0 || tail.prob > 1)
        stop("tail.prob must be between 0 and 1")
    
    n.w = nrow(w)
    
    ## portfolio VaR and ES with all assets
    r.p = bootData %*% w
    if (VaR.method == "HS") {
        pVaR = quantile(r.p, prob=tail.prob)
        idx = which(r.p <= pVaR)
        pES = -mean(r.p[idx])
    } else {
        pVaR = -VaR.CornishFisher(r.p, p=(1-tail.prob))
        idx = which(r.p <= pVaR)
        pES = -mean(r.p[idx])
    }
    ##
    ## compute marginal ES
    ##
    if (method=="derivative") {
        ## compute marginal ES as derivative wrt portfolio weight
        temp.w = w
        mES = matrix(0, n.w, 1)
        for (i in 1:n.w) {
            ## increment weight for asset i by delta.w
            temp.w[i,1] = w[i,1] + delta.w
            temp.r.p = bootData %*% temp.w
            if (VaR.method == "HS") {
                pVaR.new = quantile(temp.r.p, prob=tail.prob)
                idx = which(temp.r.p <= pVaR.new)
                pES.new = -mean(temp.r.p[idx])
            } else {
                pVaR.new = -VaR.CornishFisher(temp.r.p, p=(1-tail.prob))
                idx = which(temp.r.p <= pVaR.new)
                pES.new = -mean(temp.r.p[idx])
            }
            mES[i,1] = (pES.new - pES)/delta.w
            ## reset weight
            temp.w = w
        }
    } else {
        ## compute marginal ES as expected value of fund return given portfolio
        ## return is less than or equal to portfolio VaR
        if (ncol(bootData) > 1) {
            mES = -as.matrix(colMeans(bootData[idx,]))
        } else {
            mES = -as.matrix(mean(bootData[idx, ]))
        }
    }
    ## compute correction factor so that sum of weighted marginal ES adds to portfolio VaR
    cf = as.numeric( pES / sum(mES*w) )
    ## mES = cf*mES
    
    ## compute component and percent ES
    cES = mES * w
    pcES = cES/pES
    rownames(mES) = colnames(bootData)
    colnames(mES) = "MCES"
    colnames(cES) = "CES"
    colnames(pcES) = "PCES"
    
    ans = list(VaR.fm = -pVaR,
               ES.fm = pES,
               n.exceed = length(idx),
               idx.exceed = idx,
               MCES = t(mES),
               CES = t(cES),
               PCES = t(pcES))
    return(ans)
}
