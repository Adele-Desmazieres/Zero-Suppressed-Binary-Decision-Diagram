#!/usr/bin/python3

import matplotlib.pyplot as plt
import csv

def graphe_execution_times(filename) :
    nombre_bits = []
    temps_LDV = []
    temps_ADV = []
    
    with open(filename) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')
        line_count = 0
        
        for row in csv_reader:
            
            if line_count == 0:
                line_count += 1
                
            else:
                nombre_bits.append(int(row[0]))
                temps_LDV.append(float(row[1]))
                temps_ADV.append(float(row[2]))
                line_count += 1
                
        print(f'Processed {line_count} lines.')
    
    plt.plot(nombre_bits, temps_LDV, label="LDV")
    plt.plot(nombre_bits, temps_ADV, label="ADV")
    plt.legend()
    
    axes = plt.gca()
    axes.xaxis.set_major_locator(plt.MaxNLocator(11))
    axes.yaxis.set_major_locator(plt.MaxNLocator(11))
    
    plt.title("Temps d'exécution de la compression par liste ou par arbre,\n en fonction du nombre de bits max de la génération aléatoire")
    plt.grid()
    plt.show()
    plt.close()



def graphe_compression(filename) :
    nb_bits = []
    nb_noeuds = []
    taux_compression_LDV = []
    taux_compression_ADV = []
    
    with open(filename) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')
        line_count = 0
        
        for row in csv_reader:
            
            if line_count == 0:
                line_count += 1
                
            else:
                v1 = int(row[0])
                avant = int(row[1])
                apresLDV = int(row[2])
                apresADV = int(row[3])
                nb_bits.append(v1)
                taux_compression_LDV.append(avant / apresLDV * 100)
                taux_compression_ADV.append(avant / apresADV * 100)
                line_count += 1
                
        print(f'Processed {line_count} lines.')
    
    plt.plot(nb_bits, taux_compression_LDV, label="LDV")
    plt.plot(nb_bits, taux_compression_ADV, label="ADV")
    plt.legend()
    
    axes = plt.gca()
    axes.xaxis.set_major_locator(plt.MaxNLocator(11))
    axes.yaxis.set_major_locator(plt.MaxNLocator(11))
    
    plt.title("Taux de compression (taille avant / taille après) en nombre de nœuds dans l'arbre,\n en fonction du nombre de bits de départ")
    plt.grid()
    plt.show()
    plt.close()


graphe_execution_times('execution_times.csv')
graphe_compression('compression_rates.csv')