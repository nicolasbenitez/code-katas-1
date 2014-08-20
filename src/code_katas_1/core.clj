(ns code-katas-1.core)

(defn filter-odd
  "Escribir una funcion que retorne solamente los numeros impares de
   una secuencia"
  [s]
  (filter odd? s)
  )

(defn nil-key
  "Escribir una funcion que dada una clave y un mapa, devuelva true, solamente si el mapa
   contiene una entrada con esa clave, y su valor es nil"
  [k m]
  (if (contains? m k) (= (get m k) nil) false)
  )

(defn implement-range
  "Escribir una funcion que cree una lista de enteros en un rango dado.
   Restricciones: range"
  [start end]
  (def lis nil)
  (loop [x start]
  (when (< x end)
    (def lis (conj lis x))
    (recur (+ x 1)))) 
  (reverse lis)
  )

(defn compress-sequence
  "Escribir una funcion que elimine los duplicados consecutivos
   de una secuencia"
  [s]
  (def lista '())
  (dotimes [n (count s)]  
  (if (not=  (first lista) (get s n)) (def lista (conj lista (get s n)))))
  (reverse lista)
  )

(defn max-value
  "Escribir una funcion que reciba un numero variable de parametros
   y retorne el que tenga el valor mayor
   Restricciones: max y max-key"
  [& args]
  (first (sort > (seq (args))))
  )

(defn split-two
  "Escribir una funcion que parta una secuencia en dos partes
   Restricciones: split-at"
  [length s]
  [(take length s) (drop length s)]
  )

(defn inter-two
  "Escribir una funcion que reciba dos secuencias y retorne el primero de cada una,
   luego el segundo de cada una, luego el tercero, etc.
   Restricciones: interleave"
  [s1 s2]
  (lazy-seq
      (let [a (seq s1) b (seq s2)]
        (when (and a b)
          (cons (first a) (cons (first b) (interleave (rest a) (rest b)))))
       ))
  )

(defn retrieve-caps
  "Escribir una funcion que reciba un string y devuelva un nuevo string conteniendo
   solamente las mayusculas."
  [text]
  (filter #(Character/isUpperCase %) text)
  )

(defn find-truth
  "Escribir una funcion que tome un numero variable de booleans, y devuelva true
   solamente si alguno de los parametros son true, pero no todos son true. En otro
   caso debera retornar false"
  [& xs]
  (if (some false? xs) (if(some true? xs) true false) false)
  )

(defn zip-map
  "Escribir una funcion que reciba un vector de claves y un vector de valores, y
   construya un mapa a partir de ellos.
   Restricciones: zipmap"
  [k v]
  ((fn maprec [mapa k2 v2]
     (if (and (empty? k2) (empty? v2)) mapa 
     (maprec (assoc mapa (first k2) (first v2)) (rest k2) (rest v2))))
     {} 
     (if (> (count k)(count v))(rest(reverse k)) (reverse k)) 
     (if (< (count k) (count v)) (rest(reverse v)) (reverse v)))
  ) 