# Klasifikasi Backpropagation Neural Network
  Backpropagation Neural Network atau yang disebut dengan propagasi balik jaringan saraf tiruan termasuk metode pelatihan supervised dan didesain untuk operasi pada jaringan feedforward multi lapis (Puspitaningrum, 2006). Terdapat 3 lapisan yaitu input layer, hidden layer, dan error layer. 
# Langkah analisis data
Teknik analisis data dalam penelitian ini adalah sebagai berikut
1.	Melakukan pengumpulan data
2.	Cleaning data
3.	Menginput data
4.	Melakukan Standarisasi data
5.	Melakukan pengelompokan dengan menggunakan metode k-means
	a.	Menentukan nilai k sebagai jumlah kelompok yang ingin dibentuk
	b.	Inisiasi k sebagai centroid awal secara acak
	c.	Menghitung jarak dengan menggunakan jarak Euclidean sesuai dengan Persamaan
	d.	Mengelompokkan setiap data berdasarkan jarak terdekat dengan centroid
	e.	Menentukan posisi centroid baru (k)
	f.	Kembali ke langkah 3 jika posisi centroid baru tidak sesuai dengan centroid lama.
6.	Melakukan penerapan algoritma Backpropagation Neural Network
	a.	Pembagian data training dan testing
	b.	Input Parameter berdasarkan training dan testing
	c.	Menentukan masing-masing Hidden Neuron, Galat Tolerance, Learning rate secara trial and error.
	d.	Menghitung backpropagation neural network berdasarkan data training yang di peroleh
	e.	Menghitung akurasi model berdasarkan data testing.
7.	Penarikan kesimpulan

a.	Training dan Testing
	Sebelum melakukan pemodelan, terlebih dahulu data dibagi menjadi dua yaitu data testing dan data training. Karena tidak adanya aturan pasti dalam pembagian seberapa banyak data training dan seberapa banyak data testing yang akan digunakan, sehingga pada penelitian ini kita akan melakukan 4 kali pembagian. Pertama kita akan membagi 50% data training dan 50% data testing, kedua membagi 60% sebagai data training dan 40% data testing, ketiga membagi 70% sebagai data training dan 30% data testing dan keempat membagi 80% sebagai data training dan 20% data testing.
b.	Penentuan Hidden Layer dan Learning rate
	Sebelum menghitung akurasi dari model backpropagtion neural network, maka perlu dilakukan penentuan hidden layers dan Learning rate yang akan digunakan. Dalam penentuan hidden layers dan Learning rate dilakukan berdasarkan coba-coba (trial and error) karena tidak adanya aturan yang pasti mengenai nilai learning rate dan hidden layer, semakin rendah nilai error yang di peroleh maka semakin baik pula model yang akan didapatkan. Gambar 4.9 menunjukkan nilai error dari 4 jenis hidden layer dan learning rate yang berbeda. 

![image](https://user-images.githubusercontent.com/116243989/197651257-d7972b24-a35f-4e86-8b87-d6f79e47b95c.png)

Berdasarkan hasil analisis klasifikasi Backpropagation Neural Network yang telah dilakukan, nilai hidden layer terbaik adalah (8,6,3,4) dengan learning rate 0.001 karena memiliki nilai error paling kecil yaitu 0,021058. Hidden layer (8,6,3,4) merupakan hidden layer yang memiliki 4 layer yaitu layer pertama menggunakan 8 node, pada layer kedua menggunakan 6 node, layer ketiga menggunakan 3 node dan pada layer keempat menggunakan 4 node. 

![image](https://user-images.githubusercontent.com/116243989/197651325-3e87de14-2917-4e3a-badc-0d07662f50d1.png)

