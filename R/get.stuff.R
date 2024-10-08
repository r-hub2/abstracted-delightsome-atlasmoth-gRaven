# edits to allow logical states

get.states<-function (domain, nodes=domain$nodes)
{
# assuming logicals coded as 0 and 1 in the order specified in add.node (differs from Hugin?)
if(length(nodes)==1)
{
x<-domain$states[[nodes]]
y<-attr(x,'logical')
if(is.null(y)) x else y[x+1]
}
else
sapply(nodes,function(x) get.states(domain,x))
}

.children<-function(domain,node) domain$nodes[which(sapply(domain$parents, function(x) node%in%x))]

get.children<-function(domain,nodes)
{
if(length(nodes)==1) return(.children(domain,nodes))
else 
{
res<-list()
for(node in nodes) res[[node]]<-.children(domain, node)
res
}
}

get.edges<-function (domain, nodes = domain$nodes) 
{
    if (length(nodes) == 1) 
        return(list(edges=.children(domain, nodes)))
    else {
        res <- list()
        for (node in nodes) res[[node]] <- list(edges=.children(domain, 
            node))
        res
    }
}



