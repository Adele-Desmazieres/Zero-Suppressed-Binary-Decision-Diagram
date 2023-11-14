import matplotlib.pyplot as plt
import csv


nombre_bits = []
temps_LDV = []
temps_ADV = []

with open('execution_times.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    line_count = 0
    
    for row in csv_reader:
        
        if line_count == 0:
            line_count += 1
            
        else:
            nombre_bits.append(row[0])
            temps_LDV.append(float(row[1]))
            temps_ADV.append(float(row[2]))
            line_count += 1
            
    print(f'Processed {line_count} lines.')

print(temps_LDV)

plt.plot(nombre_bits, temps_LDV, label="LDV")
plt.plot(nombre_bits, temps_ADV, label="ADV")
plt.legend()

axes = plt.gca()
axes.xaxis.set_major_locator(plt.MaxNLocator(11))
axes.yaxis.set_major_locator(plt.MaxNLocator(11))

plt.title("Temps d'exécution de la compression par liste ou par arbre,\n en fonction du nombre de bits max de la génération aléatoire")
plt.grid()
plt.show()
