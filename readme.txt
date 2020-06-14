This is a tutorial for generate 3-tired web applications for SFC experiments used in CloudSimSDN.

Author:
Jay Son (Jungmin Son)
TianZhang He
at CLOUDS Lab, University of Melbourne.

1. wiki_workload_generator_complex_SFC.R
run function "main_history" to generate the workload for SFC expeirment used in CloudSimSDN.
Arrival rate: following the wikipedia trace. (A template trace file "overallstatpersec9-1-sfc.csv" are included.)
More wikipedia access trace can be found at http://www.wikibench.eu/?page_id=60.

# Parameters for the 3-tier web application includes webNum, appNum, dbNum.

We assumed that Load Balancer already worked, so that the workload is assigned in round-robin.
Two kinds of workoad
1. Cached workload which only arrives at Web server, no access to App or DB.
   This implies the Web server had a cached data.
2. Traditional worklaod that passes Web, App, and DB servers.

The history based workloads generations are based on the paper:
Ersoz, Deniz, Mazin S. Yousif, and Chita R. Das. "Characterizing network traffic in a cluster-based, multi-tier data center." 27th International Conference on Distributed Computing Systems (ICDCS'07). IEEE, 2007.

# Parameters for history based workloads
NETWORK_PACKET_SIZE_MULTIPLY
CPU_MULTIPLY
NUM_WORKLOAD_RATE_SCALE

#average packet size between different types of servers: FRONT(web) MID(app) BACK(bd)
PACKETSIZE_FRONT_MID
PACKETSIZE_MID_BACK
PACKETSIZE_BACK_MID
PACKETSIZE_MID_FRONT

The output results will be a list of workloads "0_appnumid_workload_wiki.csv".

2. In CloudSimSDN package "example.topogenerators", use "generateLarge3TierTopologySFC" function within "VirtualTopologyGeneratorVmTypesSFC.java" 
to generate the corresponding "sfc.virtual.json" for the SFC experiment.

Configurate the parameters numWeb, numApp, numDB, linkBw in "generateLarge3TierTopologySFC" function.
and corresponding vnf types and SFC policies in "createSFCPolicy" function.

