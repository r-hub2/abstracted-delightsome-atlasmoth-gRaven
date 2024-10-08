print.gRaven<-function(x, ...)
{
      cat("A gRaven domain with slots:",names(x),"\n")
      cat(length(x$nodes),"nodes:",x$nodes,fill=60)
      cat(sum(sapply(x$parents,length)),"edges:\n")
      for(n in x$nodes) if(length(x$parents[[n]])>0) cat("  ",x$parents[[n]],"->",n,"\n")
      invisible(NULL)
}

summary.gRaven<-function(object, ...)
{
      cat('gRaven domain:\n')
      res1<-if(!is.null(object$net)) object$net$isCompiled else FALSE
      cat(paste0('    Compiled: the domain is ',ifelse(res1,'','not '),'compiled\n'))
      res2<-if(!is.null(object$net)) object$net$isPropagated else FALSE
      cat('  Propagated:',res2,'\n')
# JT
      JT<-if(!is.null(object$net)) object$net$rip$cliques else NULL
      invisible(print(JT))
}


hugin.domain<-function () 
{
	e <- rlang::env( nodes=NULL, states=NULL, parents=NULL, cptables=NULL)
	class(e)<-c("gRaven","environment")
	e
}

clone.domain<-function(domain)
	{
	domain2<-rlang::env_clone(domain)
	class(domain2)<-c("gRaven","environment")
	domain2
	}

initialize.domain<-function(domain)
	{
	if(is.null(domain$net)) return()
	domain$net<-retractEvidence(domain$net)
	domain$net$cache<-NULL
	}

add.node<-function (domain, name, category = c("chance", 
    "decision", "utility", "function"), kind = c("discrete", "continuous", "other"), 
    subtype, states) 
{
    category <- match.arg(category)
    if(category!="chance") stop("gRaven does not yet handle category =",category)
    kind <- match.arg(kind)
    if(kind!="discrete") stop("gRaven does not yet handle kind =",kind)
    if (missing(states)) {
        if (subtype == "boolean") 
            states <- c(0, 1)
		attr(states,"logical")<-c(FALSE,TRUE)
    }
    else {
        if (missing(subtype)) 
            subtype <- switch(mode(states), character = "labeled", 
                numeric = "numbered", logical = "boolean")
    }
    if(mode(states)=="logical")  
		{
		attr(states,"logical")<-states
		states[] <- c(0, 1)
		}
    if (name %in% domain$nodes) 
        stop(name, " already in domain\n")
    if(any(duplicated(states))) stop("states must be distinct")
    
    domain$nodes <- c(domain$nodes, name)
    domain$states <- c(domain$states, structure(list(states), names = name))
    domain$parents <- c(domain$parents, structure(list(NULL), names = name))
}

delete.node<-function (domain, name) 
{
    domain$nodes <- domain$nodes[domain$nodes != name]
    domain$states[[name]] <- NULL
    domain$parents[[name]] <- NULL
    domain$cptables[[name]] <- NULL
	for(m in domain$nodes) if(name%in%domain$parents[[m]]) 
	{
	pars<-domain$parents[[m]]
	pars<-pars[pars!=name]
	if(length(pars)==0) domain$parents[m]<-list(NULL) else domain$parents[[m]]<-pars
	domain$cptables[[m]]<-NULL
	}
}

add.edge<-function(domain,child,parent)
{
if((!child%in%domain$nodes)||any(!parent%in%domain$nodes)) stop(child,"",parent," not all already in domain\n")
domain$parents[[child]]<-c(domain$parents[[child]],parent)
domain$cptables[[child]]<-NULL
}

delete.edge<-function (domain, child, parent) 
{
    pars <- setdiff(domain$parents[[child]],parent)
    if (length(pars) == 0) 
        domain$parents[child] <- list(NULL)
    else domain$parents[[child]] <- pars
    domain$cptables[[child]] <- NULL
}

get.table<-function (domain,n,type = c("cpt", "experience", 
    "fading"), class = c("data.frame", "table", 
    "ftable", "numeric")) 
{
# delivers CPT as a data.frame, either by extracting it if it already exists in domain$cptables, 
# or initialised with freq=1
type <- match.arg(type)
if(type!="cpt") stop("gRaven does not yet handle type =",type)
class <- match.arg(class)
if(class!="data.frame") stop("gRaven does not yet handle class =",class)
if(!is.null(domain$net))
{
return(as.data.frame.table(domain$net$cpt[[n]]))
}
z<-domain$cptables
if(is.null(z)||is.null(z[[n]])) 
{
        Freq<-1
        vpa<-c(n,domain$parents[[n]])
} else {
        Freq<-z[[n]]
        vpa<-names(dimnames(Freq))
}
allstates<-domain$states[vpa]
nlev<-unlist(lapply(allstates,length))
k<-cumprod(nlev)
lk<-length(k); length<-k[lk]; k<-c(1,k[-lk])
for(i in 1:lk) 
{
        w<-allstates[[i]][as.numeric(gl(nlev[i],k[i],length))]
        if(i==1) df<-data.frame(w) else df<-cbind(df,w)
}
df<-cbind(df,as.vector(Freq))
names(df)<-c(vpa,"Freq")
df
}

set.table<-function(domain,n,tab=1,type = c("cpt", "experience", 
    "fading"))
{
type <- match.arg(type)
if(type!="cpt") stop("gRaven does not yet handle type =",type)
if(is.data.frame(tab)) Freq<-tab$Freq else Freq<-as.vector(tab)
if(is.null(domain$net))
{
	if(is.null(domain$cptables)||is.null(domain$cptables[[n]]))
	{
		vpa<-c(n,domain$parents[[n]])
		allstates<-domain$states[vpa]
		nlev<-unlist(lapply(allstates,length))
		leng<-prod(nlev)
		domain$cptables[[n]]<-cpt(vpa,values=rep_len(Freq,leng),levels=allstates)
	} else {
		domain$cptables[[n]][]<-Freq
	}
} else {
	domain$net<-replaceCPT(domain$net,structure(list(Freq),names=n))
}
}

compile.gRaven<-function(object, ...)
	{
	if(!is.null(object$net)) warning("domain already compiled: net is being re-initialised")
# if any nodes are missing cptables, provide dummy table
	for(n in setdiff(object$nodes,names(object$cptables))) {
		vpa<-c(n,object$parents[[n]])
		allstates<-object$states[vpa]
		nlev<-unlist(lapply(allstates,length))
		leng<-prod(nlev)
		object$cptables[[n]]<-cpt(vpa,values=rep_len(1,leng),levels=allstates)
	}
	net<-grain(compileCPT(object$cptables))
	class(net)<-c("cpt_grain","grain")
	object$net<-net
	}

check.compiled<-function(object)
{
	if(!all(object$nodes%in%names(object$cptables))) {
		if(is.null(object$cptables)) object$cptables<-list()
		for(n in object$nodes) if(is.null(object$cptables[[n]])) {set.table(object,n,1)}
		object$net<-NULL
	}
	if(is.null(object$net)) {compile.gRaven(object); cat("now compiled\n")}
}

set.finding<-function(domain, node, finding)
	{
	check.compiled(domain)
	domain$net$isPropagated<-FALSE

	if (is.list(finding)) finding<-unlist(finding)
	if (length(finding) == 1) finding <- as.integer(finding == domain$states[[node]])

	cache<-domain$net$cache
	if(is.null(cache)) cache<-list()

# if it exists, empty evid into cache (E dominates over existing C if any, but new evidence dominates E)
	if(!is.null(domain$net$evidence))
			{
			e<-domain$net$evidence$evi_weight
			for(i in 1:length(e))
				{
				n<-names(dimnames(e[[i]]))
				cache[[n]]<-as.vector(e[[i]])
				}
				domain$net<-retractEvidence(domain$net,domain$net$evi$nodes,propagate=FALSE)
			}

	cache[[node]]<-finding
	domain$net$cache<-cache
	}

retract<-function(domain, nodes=domain$nodes)
	{
	if(is.null(domain$net)) return(invisible(NULL))
	if(!is.null(domain$net$cache)) 
	{
		nret<-intersect(nodes,names(domain$net$cache))
		if(length(nret)>0) {
			domain$net$cache[nret]<-NULL
			if(length(domain$net$cache)==0) domain$net$cache<-NULL
		}
	}
	if(!is.null(domain$net$evidence)) domain$net<-retractEvidence(domain$net,intersect(nodes,domain$net$evi$nodes),propagate=FALSE)
	}

get.finding<-function (domain, nodes = domain$nodes, type = c("entered", "propagated"), 
    namestates = FALSE) 
{
    type <- match.arg(type)
    res <- list()
    for (i in seq_along(domain$net$evi$evi)) {
        node <- names(dimnames(domain$net$evi$evi[[i]]))
        finding <- as.vector(domain$net$evi$evi[[i]])
        names(finding) <- get.states(domain, node)
        if (node %in% nodes) 
		{            
		if (namestates) {
                cat(paste0(node, ":"), "evid\n")
                print(finding)
            } else {
                cat(paste0(node, ":"), finding, "evid\n")
            }
		res[[node]] <- finding
	}
    }
    if (type != "propagated") 
        for (i in seq_along(domain$net$cache)) {
            node <- names(domain$net$cache)[i]
            finding <- domain$net$cache[[i]]
            names(finding) <- get.states(domain, node)
            if (node %in% nodes) 
		{
             if (namestates) {
                  cat(paste0(node, ":"), "cache\n")
                  print(finding)
             } else {
                  cat(paste0(node, ":"), finding, "cache\n")
             }
             res[[node]] <- finding
		}
        }
	res<-if (length(res) == 1) res[[1]] else 
		if(length(res)==0 & length(nodes)==1) structure(rep(1,length(get.states(domain,nodes))),names=get.states(domain,nodes)) else 
		res
    invisible(res)
}

get.marginal<-function (domain, nodes, class = c("data.frame", "table", "ftable", 
    "numeric")) 
{
class <- match.arg(class)
z<-querygrain(domain$net, nodes, "joint", exclude = FALSE, evidence = domain$net$cache)
z<-aperm(z,nodes)
res<-switch(class,
data.frame={
	res <- as.data.frame.table(z)
	res<-res[, c(nodes, "Freq")]
	for (node in nodes) {
		res[[node]] <- get.states(domain, node)[as.integer(res[[node]])]
		}
      res},
table=z,
ftable=if(length(nodes)>1) ftable(z, row.vars = nodes[1:floor((length(nodes)+1)/2)]) else z,
numeric=as.vector(z)
)
list(table = res)
}

get.belief<-function(domain,nodes)
{
if(length(nodes)>1)
{
return(get.marginal(domain,nodes))
} else {
structure(as.vector(querygrain(domain$net, nodes, exclude=FALSE, evidence = domain$net$cache)[[1]]),names=get.states(domain,nodes))
}
}

propagate.gRaven<-function(object, ...) 
	{
	check.compiled(object)
	if(!is.null(object$net$cache))
		{
		net1<-setEvidence(object$net,evidence=object$net$cache,propagate=TRUE)
		net1$cache<-NULL
		} else {
		net1<-propagate(object$net)
		}
	object$net<-net1
	}

get.normalization.constant<-function(domain,log=FALSE) 
	{
	if(!is.null(domain$net$cache))
		{
		if(!is.null(domain$net$evidence))
# C&E (if both hold evidence on a node, E dominates)
			{
			e<-domain$net$evidence$evi_weight
			for(i in 1:length(e))
				{
				n<-names(dimnames(e[[i]]))
				domain$net$cache[[n]]<-as.vector(e[[i]])
				}
			domain$net$evidence<-NULL
			}
		p<-pEvidence(domain$net,evidence=domain$net$cache)		
		} else if(is.null(domain$net$evidence)) {
# C only
		p<-1
		} else {
# E only or neither
		if(domain$net$isPropagated) 
			p<-pEvidence(domain$net) else
			{
			p<-pEvidence(propagate(domain$net))
			}
		} 
	if(log) log(p) else p
	}

get.nodes<-function(domain) domain$nodes

get.parents<-function(domain, n, type = "parents")
{
if(type!="parents") stop("gRaven does not yet handle type =",type)
domain$parents[[n]]
}
    
simulate.gRaven <- function(object, nsim = 1, seed = NULL, ...)
	{
	simulate.grain(object$net, nsim = nsim, seed = NULL, ...)   
	}

triangulate.gRaven<-function(object, ...) {}

compress<-function(domain) {1}

list.domains<-function (print = TRUE) 
{
	domains <- NULL
	lsa<-ls(all.names = TRUE, envir = .GlobalEnv)
	for (x in lsa) if (is(get(x), "gRaven")) domains <- c(domains, x)
	if(print) cat(domains,fill=60)
	for(v in lsa) if(is.list(get(v))&&"domains"%in%names(get(v))) 
		{
		if(print) cat(paste0(" ",v,"$domains$"),names(get(v)$domains),"\n")
		domains<-c(domains,paste0(v,"$domains$",names(get(v)$domains)))
		}
invisible(domains)
}

