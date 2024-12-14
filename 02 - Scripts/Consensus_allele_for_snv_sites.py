# ====== Import Things ======
import os
import pandas as pd
from Bio import SeqIO
from Bio.Seq import Seq
from Bio.SeqRecord import SeqRecord


# ================================ Main =========================================

consensus = "Brisbane_H1N1_2019.all.consensus.fasta"
sites = (10392,10856,12143,12271,12789,13121,13143,13429,7397,7509,7543,7669,9399)
f = open("consensus_allele_forsnv_sites.txt", "a")
header = "sample Allele POS"
f.write (header + '\n')

for record in SeqIO.parse(consensus, "fasta"):
    for site in sites:
         bp=(record.id, record.seq[site-1], site)
         f.write (' '.join(str(s) for s in bp) + '\n')
f.close ()    
