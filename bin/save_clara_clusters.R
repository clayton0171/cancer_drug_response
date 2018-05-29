#!/usr/bin/R

for(clust in 180:220) {
        clust = toString(clust)
        c = clusterResults(gem_cluster_final, method="clara");
	c2 = c[[clust]][['clustering']]
	write.table(c2, file=paste("gem_multi_5k","_clara_",clust,"_cluster_assignments.txt",sep = ""), quote=FALSE, col.names=FALSE)
}

