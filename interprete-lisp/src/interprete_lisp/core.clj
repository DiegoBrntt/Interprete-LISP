(ns interprete-lisp.core
  (:gen-class))

(require '[clojure.string :refer [blank? ends-with? lower-case]] '[clojure.java.io :refer [reader]])

(defn spy
  ([x] (do (prn x) (prn) x))
  ([msg x] (do (print msg) (print ": ") (prn x) (prn) x))
)

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-de)
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-load)
(declare evaluar-setq)
(declare evaluar-quote)
(declare evaluar-lambda)
(declare evaluar-escalar)

; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-ge)
(declare fnc-gt)
(declare fnc-lt)
(declare fnc-add)
(declare fnc-env)
(declare fnc-not)
(declare fnc-sub)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-null)
(declare fnc-read)
(declare fnc-rest)
(declare fnc-equal)
(declare fnc-first)
(declare fnc-listp)
(declare fnc-prin3)
(declare fnc-append)
(declare fnc-length)
(declare fnc-terpri)
(declare fnc-reverse)

; Funciones auxiliares
(declare buscar)
(declare error?)
(declare igual?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare actualizar-amb)
(declare controlar-aridad)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-en-cond)
(declare evaluar-secuencia-en-cond)

(defn -main
  [& args]
  (repl))

; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua. El resultado es una lista con un valor y un ambiente. 
; Si la 2da. posicion del resultado es nil, devuelve true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
  "Inicia el REPL de TLC-LISP."
  ([]
   (println "Interprete de TLC-LISP en Clojure")
   (println "Inspirado en:")
   (println "  TLC-LISP Version 1.51 for the IBM Personal Computer")
   (println "  Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
   (repl '(add add append append cond cond cons cons de de env env equal equal
               eval eval exit exit first first ge ge gt gt if if lambda lambda
               length length list list listp listp load load lt lt nil nil
               not not null null or or prin3 prin3 quote quote read read
               rest rest reverse reverse setq setq sub sub t t terpri terpri
               + add - sub)))
  ([amb]
   (print ">>> ") (flush)
   (try
     (let [res (evaluar (read) amb nil)]  ; READ, EVAL
       (if (nil? (second res))
           true
           (do (imprimir (first res))     ; PRINT
               (repl (second res)))))     ; LOOP
   (catch Exception e
     (println) (print "*error* ")
     (println (get (Throwable->map e) :cause))
     (repl amb)))))


(defn evaluar
  "Evalua una expresion 'expre' en los ambientes global y local. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb-global amb-local]
  (if (or (igual? expre nil)
          (and (seq? expre)
               (or (empty? expre) (error? expre)))) ; si 'expre' es nil, () o error, devolverla intacta
      (list expre amb-global)                       ; de lo contrario, evaluarla
      (cond
        (not (seq? expre))             (evaluar-escalar expre amb-global amb-local)

        (igual? (first expre) 'cond)   (evaluar-cond expre amb-global amb-local)
        (igual? (first expre) 'de)     (evaluar-de expre amb-global)
        (igual? (first expre) 'if)     (evaluar-if expre amb-global amb-local)
        (igual? (first expre) 'eval)     (evaluar-eval expre amb-global amb-local)
        (igual? (first expre) 'exit)     (evaluar-exit expre amb-global nil)
        (igual? (first expre) 'lambda)     (evaluar-lambda expre amb-global nil)
        (igual? (first expre) 'eval)     (evaluar-eval expre amb-global amb-local)
        (igual? (first expre) 'load)     (evaluar-load expre amb-global amb-local)
        (igual? (first expre) 'or)     (evaluar-or expre amb-global amb-local)
        (igual? (first expre) 'quote)     (evaluar-quote expre amb-global nil)
        (igual? (first expre) 'setq)     (evaluar-setq expre amb-global amb-local)
         ;
         ;
         ;
         ; Si la expresion no es la aplicacion de una funcion (es una forma especial, una macro...) debe ser evaluada aqui
         ; por una funcion de Clojure especifica debido a que puede ser necesario evitar la evaluacion de los argumentos
         ;
         ;
         ;

        :else (let [res-eval-1 (evaluar (first expre) amb-global amb-local),
				                res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x) amb-local)] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
				               (aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2) amb-local)))))


; Evalua una macro COND. Siempre devuelve una lista con un resultado y un ambiente.
(defn evaluar-cond [expre amb-global amb-local]
  "Evalua una forma 'cond' en TLC-LISP."
   (evaluar-clausulas-en-cond (next expre) amb-global amb-local))


(defn evaluar-clausulas-en-cond [expre amb-global amb-local]
  "Une 'evaluar-cond' con 'evaluar-secuencia-en-cond'."
  (if (nil? expre)
      (list nil amb-global)
     	(let [res-eval (evaluar (ffirst expre) amb-global amb-local)]
           (cond
             (error? (first res-eval)) res-eval
             (not (igual? (first res-eval) nil)) (evaluar-secuencia-en-cond (nfirst expre) (second res-eval) amb-local)
	            :else (recur (next expre) (second res-eval) amb-local)))))


; Evalua (con evaluar) secuencialmente las sublistas de una lista y devuelve el valor de la ultima evaluacion.
; Si alguna evaluacion devuelve un error, sera la ultima que se evalue. 
(defn evaluar-secuencia-en-cond [lis amb-global amb-local]
	(if (nil? (next lis))
	    (evaluar (first lis) amb-global amb-local)
	    (let [res-eval (evaluar (first lis) amb-global amb-local)]
	         (if (error? (first res-eval))
   		         res-eval
  	           (recur (next lis) (second res-eval) amb-local)))))


(defn evaluar-eval
  "Evalua una forma 'eval' en TLC-LISP."
  [expre amb-global amb-local]
  (let [ari (controlar-aridad (next expre) 1)]
		     (cond
		       (seq? ari) ari
         (and (seq? (second expre)) (igual? (first (second expre)) 'quote)) (evaluar (second (second expre)) amb-global amb-local)
         :else (evaluar (second expre) amb-global amb-local))))


(defn evaluar-exit
  "Sale del interprete de TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list nil nil)
    :else (list (list '*error* 'too-many-args) amb-global)))


(defn evaluar-lambda
  "Evalua una forma 'lambda' en TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
    (and (not (igual? (second expre) nil)) (not (seq? (second expre)))) 
      (list (list '*error* 'list 'expected (second expre)) amb-global)
    :else (list expre amb-global)))


(defn evaluar-load
  "Evalua una forma 'load' en TLC-LISP. Carga en el ambiente un archivo 'expre' con código en TLC-LISP."
  [expre amb-global amb-local]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'too-few-args) amb-global)
				(> (count (next expre)) 1) (list (list '*error* 'not-implemented) amb-global)
			 :else (list \space (cargar-arch amb-global amb-local (second expre)))))


(defn cargar-arch
  ([amb-global amb-local arch]
    (let [nomb (first (evaluar arch amb-global amb-local))]
      (if (error? nomb)
	         (do (imprimir nomb) amb-global) 
          (let [nm (clojure.string/lower-case (str nomb)),
                nom (if (and (> (count nm) 4) (clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp")),
                ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                           (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
							                                                      (cargar-arch (second res) nil in res))
	                                                       (catch Exception e (imprimir nil) amb-global))))
			  	              (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
  		           ret))))
  ([amb-global amb-local in res]
    (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (second res) nil in res))
    (catch Exception e (imprimir (first res)) amb-global)))
)


(defn evaluar-quote
  "Evalua una forma 'quote' de TLC-LISP."
  [expre amb-global _]
  (if (igual? (second expre) nil)
    (list nil amb-global)
    (list (second expre) amb-global)))


(defn aplicar
  "Aplica a la lista de argumentos 'lae' la función 'fnc' en los ambientes dados."
  ([fnc lae amb-global amb-local]
   (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb-global amb-local))
  ([resu1 resu2 fnc lae amb-global amb-local]
   (cond
     (error? resu1) (list resu1 amb-global)
     (error? resu2) (list resu2 amb-global)
     (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb-global amb-local) amb-global)
     :else (aplicar-lambda fnc lae amb-global amb-local))))


(defn aplicar-lambda
  "Aplica la forma lambda 'fnc' a la lista de argumentos 'lae'."
  [fnc lae amb-global amb-local]
  (cond
    (< (count lae) (count (second fnc))) (list '(*error* too-few-args) amb-global)
    (> (count lae) (count (second fnc))) (list '(*error* too-many-args) amb-global)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb-global amb-local)
    :else (aplicar-lambda-multiple fnc lae amb-global amb-local)))


(defn aplicar-lambda-simple
  "Evalua una forma lambda 'fnc' con un cuerpo simple."
  [fnc lae amb-global amb-local]
  (evaluar (first (nnext fnc)) amb-global (concat (reduce concat (map list (second fnc) lae)) amb-local)))


(defn aplicar-lambda-multiple
  "Evalua una forma lambda 'fnc' cuyo cuerpo contiene varias expresiones."
  [fnc lae amb-global amb-local]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
           lae
           (second (aplicar-lambda-simple fnc lae amb-global amb-local))  ; Nuevo ambiente global
           amb-local))


(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una 'lae' (lista de argumentos evaluados)."
  [fnc lae amb-global amb-local]
  (cond
    (igual? fnc 'add)     (fnc-add lae)
    (igual? fnc 'append)     (fnc-append lae)
    (igual? fnc 'cons)     (fnc-cons lae)
    (igual? fnc 'env)     (fnc-env lae amb-global amb-local)
    (igual? fnc 'equal)     (fnc-equal lae)
    (igual? fnc 'first)     (fnc-first lae)
    (igual? fnc 'ge)     (fnc-ge lae)
    (igual? fnc 'gt)     (fnc-gt lae)
    (igual? fnc 'length)     (fnc-length lae)
    (igual? fnc 'list)     (fnc-list lae)
    (igual? fnc 'listp)     (fnc-listp lae)
    (igual? fnc 'lt)     (fnc-lt lae)
    (igual? fnc 'not)     (fnc-not lae)
    (igual? fnc 'null)     (fnc-null lae)
    (igual? fnc 'prin3)     (fnc-prin3 lae)
    (igual? fnc 'read)     (fnc-read lae)
    (igual? fnc 'rest)     (fnc-rest lae)
    (igual? fnc 'reverse)     (fnc-reverse lae)
    (igual? fnc 'sub)     (fnc-sub lae)
    (igual? fnc 'terpri)     (fnc-terpri lae)
    (igual? fnc '+)     (fnc-add lae)
    (igual? fnc '-)     (fnc-sub lae)

    ; Las funciones primitivas reciben argumentos y retornan un valor (son puras)

    :else (list '*error* 'non-applicable-type fnc)))


(defn fnc-cons
  "Devuelve la inserción de un elem en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad lae 2)]
			 (cond
			   (seq? ari) ari
		    (or (seq? (second lae)) (igual? (second lae) nil)) (cons (first lae) (second lae))
			   :else (list '*error* 'not-implemented))))


(defn fnc-first
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (igual? (first lae) nil) nil
      (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
      :else (ffirst lae))))


(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (or (seq? (first lae)) (igual? (first lae) nil)) (count (first lae))
      :else (list '*error* 'arg-wrong-type (first lae)))))


(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1) nil lae))


(defn fnc-listp
  "Devuelve 't' si un elemento es una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (seq? (first lae)) 't
      :else nil)))


(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
			   (igual? (first lae) nil) 't
					 :else nil)))


(defn fnc-null
  "Devuelve 't' si un elemento es 'nil' en TLC-Lisp."
  [lae]
  (fnc-not lae))


(defn fnc-prin3
  "Imprime un elemento y lo devuelve."
  [lae]
  (cond
    (< (count lae) 1) (list '*error* 'too-few-args)
				(> (count lae) 1) (list '*error* 'not-implemented)
				(not (seq? (first lae))) (do (print (first lae)) (flush) (first lae))
				:else (do (print (map #(if (igual? % nil) nil %) (first lae))) (flush) (first lae))))


(defn fnc-rest
  "Devuelve una lista sin su 1ra. posición."
  [lae]
  (let [ari (controlar-aridad lae 1)]
				(cond
				  (seq? ari) ari
						(igual? (first lae) nil) nil
						(not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
						:else (nfirst lae))))


(defn imprimir
   "Imprime, con un salto de linea al final, lo recibido devolviendo 
    el mismo valor. Tambien muestra los errores."
   ([elem]
	    (cond
	      (not (seq? elem)) (if (igual? elem \space)
	                            (do (flush) elem)
	                            (do (prn (if (igual? elem nil) nil elem)) (flush) elem))
       (error? elem) (imprimir elem elem)
       :else (do (prn (map #(if (igual? % nil) nil %) elem)) (flush) elem)))
   ([lis orig]
      (if (nil? lis)
	         (do (prn) (flush) orig)
		        (do (pr (first lis)) (print " ") (imprimir (next lis) orig)))))

(defn controlar-aridad
  "Si la longitud de una lista dada es la esperada, devuelve esa longitud.
   Si no, devuelve una lista con un mensaje de error (una lista con *error* como primer elemento)."
   [lis ari] 
   (cond 
     (= (count lis) ari) ari
     (< (count lis) ari) (list '*error* 'too-few-args)
      :else (list '*error* 'too-many-args)
   )
)

(defn igual?
  "Verifica la igualdad entre dos elementos al estilo de TLC-LISP (case-insensitive)."
  [arg1 arg2]
  (let [res (filter #(or (= % nil) (= % 'NIL) (= % ())) (list arg1 arg2))]
  (cond
    
    (= (count res) 1) false

    (= (count res) 2) true

    (every? seq? (list arg1 arg2)) 
      (= (map #(.toUpperCase (str %)) arg1) (map #(.toUpperCase (str %)) arg2))

    (every? symbol? (list arg1 arg2)) 
      (= (.toUpperCase (str arg1)) (.toUpperCase (str arg2)))

    :else (= arg1 arg2)
  ))
)

(defn error?
  "Devuelve true o false, segun sea o no el arg. un mensaje de error (una lista con *error* como primer elemento)."
  [arg]
  (cond 
    (or
      (not (seq? arg))
      (igual? arg nil)
      (nil? (re-matches #"(?i)\*error\*" (str (first arg))))
    ) false

    (not (nil? (re-matches #"(?i)\*error\*" (str (first arg))))) true
  )
)

(defn revisar-fnc
  "Si la lista es un mensaje de error, lo devuelve; si no, devuelve nil."
  [arg]
  (cond 
    (error? arg) arg
  )
)

(defn revisar-lae
  "Devuelve el primer elemento que es un mensaje de error. Si no hay ninguno, devuelve nil."
  [arg]
  (cond 
    (some error? arg) (->> (filter error? arg)
                           (first ,,,))
  )  
)


(defn actualizar-amb
  "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor. 
  Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza el valor."
  [arg clave valor]
  (let [arg-aux (partition-all 2 arg)]
  (cond 

    (error? valor)  arg

    (igual? arg nil) (list clave valor)

    (every? #(not (igual? (first %) clave)) arg-aux) (concat arg (list clave valor))
        
     :else   (->>(map #(if (igual? (first %) clave) (list (first %) valor) (list (first %) (second %))) arg-aux)
                 (apply concat ,,,)
             )
  ))
)

(defn buscar
  "Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
   y devuelve el valor asociado. Devuelve un mensaje de error si no la encuentra."
   [clave arg]
   (let [arg-aux (partition-all 2 arg)
         res (filter #(igual? (first %) clave) arg-aux)]
   (cond 
   		(not (igual? res nil)) 
   		  (second (first res))
     
     :else (list '*error* 'unbound-symbol clave)
   ))
)

(defn fnc-append
  "Devuelve el resultado de fusionar 2 sublistas."
  [lista]
   (let [res (filter #(and (not (seq? %)) (not (igual? % nil))) lista)
         aux (concat (first lista) (second lista))]
     (cond
        (not= (count lista) 2) (controlar-aridad lista 2)
        
        (not (igual? res nil)) (->> (first res)
                                    (list '*error* 'list 'expected ,,,))
        
        (not (igual? aux nil)) aux
)))


(defn fnc-env
  "Devuelve la fusion de los ambientes global y local."
  [lista global local]
  (cond 
     (igual? lista nil) (concat global local)
     :else (list '*error* 'too-many-args)
  )
)

(defn fnc-equal
  "Compara 2 elementos. Si son iguales, devuelve t. Si no, nil."
  [arg]
  (cond 
    (not= (count arg) 2) (controlar-aridad arg 2)
    (igual? (first arg) (second arg)) 't
  )
)

(defn fnc-read
  "Devuelve la lectura de un elemento de TLC-LISP desde la terminal/consola."
  [arg]
  (if (igual? arg nil)
    (let [x (read)]
      (if (igual? x nil)
        nil
        x
      )
    )
    (list '*error* 'not-implemented)
  )
)

(defn fnc-terpri
  "Imprime un salto de línea y devuelve nil."
  [arg]
  (cond 
    (empty? arg) (prn)
    :else (list '*error* 'not-implemented)
  )
) 

(defn fnc-add
  "Suma los elementos de una lista. Minimo 2 elementos."
  [arg]
  (cond 
    (< (count arg) 2) '(*error* too-few-args)
    (every? number? arg) (reduce + arg)
    :else  (->> (filter #(not (number? %)) arg) 
                (first ,,,)
                (list '*error* 'number-expected ,,,))
  )
)

(defn fnc-sub
  "Resta los elementos de un lista. Minimo 1 elemento."
  [arg]
  (cond
    (= (count arg) 0) '(*error* too-few-args)
    (< (count arg) 2) (- 0 (first arg))
    (every? number? arg) (reduce - arg)
     :else (->> (filter #(not (number? %)) arg) 
                (first ,,,)
                (list '*error* 'number-expected ,,,))
  )
)

(defn fnc-lt
   "Devuelve t si el primer numero es menor que el segundo; si no, nil."
   [arg]
   (cond
     (not= (count arg) 2) (controlar-aridad arg 2)
     (every? number? arg) (cond (< (first arg) (second arg)) 't)
     (some #(not (number? %)) arg) 
      (->> (filter #(not (number? %)) arg) 
           (first ,,,)
           (list '*error* 'number-expected ,,,))
   )
)

(defn fnc-gt
  "Devuelve t si el primer numero es mayor que el segundo; si no, nil."
  [arg]
   (cond
     (not= (count arg) 2) (controlar-aridad arg 2)
     (every? number? arg) (cond (> (first arg) (second arg)) 't)

     (some #(not (number? %)) arg) 
      (->> (filter #(not (number? %)) arg) 
           (first ,,,)
           (list '*error* 'number-expected ,,,))
   )
)

(defn fnc-ge
  "Devuelve t si el primer numero es mayor o igual que el segundo; si no, nil."
  [arg]
   (cond
     (not= (count arg) 2) (controlar-aridad arg 2)
     (every? number? arg) (cond (>= (first arg) (second arg)) 't)
     (some #(not (number? %)) arg) 
      (->> (filter #(not (number? %)) arg) 
           (first ,,,)
           (list '*error* 'number-expected ,,,))
   )
)

(defn fnc-reverse
  "Devuelve una lista con sus elementos en orden inverso."
  [arg]
  (cond 
    (not= (count arg) 1) (controlar-aridad arg 1)
    (seq? (first arg)) (reverse (first arg))
     :else (->> (first arg) 
                (list '*error* 'list 'expected ,,,))
  )
)			

(defn evaluar-escalar
  "Evalua una expresion escalar consultando, si corresponde, los ambientes local y global. Devuelve una lista con el resultado y un ambiente."
  [arg global local]
  (let [amb-total (concat local global)
         res (buscar arg amb-total)]
  (cond 
     (or
        (number? arg)
        (string? arg)
      ) (list arg global)
   
      :else  (list res global)
   )
  )
)

(defn evaluar-de
  "Evalua una forma 'de'. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
  [arg amb]
    (cond

      (<= (count arg) 2)  (list (list '*error* 'list 'expected nil) amb)

      (not (seq? (nth arg 2))) (list (list '*error* 'list 'expected (nth arg 2)) amb)
    
      (and (not (symbol? (second arg))) (not (igual? (second arg) nil)))
        (list (list '*error* 'symbol 'expected (second arg)) amb)

      (igual? (second arg) nil)
        (list (list '*error* 'cannot-set nil) amb)
      
      :else
        (list (second arg) (actualizar-amb amb (second arg) (concat (list 'lambda (nth arg 2)) (nthrest arg 3)))) 
    )
)

(defn evaluar-if
  "Evalua una forma 'if'. Devuelve una lista con el resultado y un ambiente eventualmente modificado."
  [arg global local]
  (let [res (evaluar (second arg) global local)]
  (cond
    
      (<= (count arg) 2) (list nil global)
    
    (and
      (or
        (igual? (first res) nil)
        (error? (first res))
      )
      (= (count arg) 3)
    ) res

    
    (igual? (first res) nil) (evaluar (last arg) global local) 


     :else (evaluar (nth arg 2) global local)
  ))
)

(defn evaluar-or
  "Evalua una forma 'or'. Devuelve una lista con el resultado y un ambiente actualizado."
  [arg global local]
  (let [res (evaluar (second arg) global local)]
  (cond 
      (and
           (not (igual? (drop 2 arg) nil))
           (or
           		(igual? (first res) nil)
           		(error? (first res))
           )
      ) (evaluar-or (concat '(or) (drop 2 arg)) (second res) local)

      :else res
  ))
)

(defn evaluar-setq-simple
  "Evalua una forma 'setq' simple. Devuelve una lista con el resultado y un ambiente actualizado."
  [arg global local]
  (cond
    (or
      (= (count arg) 1) ;(setq)
      (= (count (rest arg)) 1) ;(setq x)
      (and                     ;(setq nil)
       (= (count arg) 2)
       (igual? (second arg) nil)
      )
    ) (list (list '*error* 'list 'expected nil) global)

    
    (igual? (second arg) nil) ;(setq nil 7)
       (list (list '*error* 'cannot-set nil) global)

    
    (not (symbol? (second arg))) ;(setq 7 8)
       (list (list '*error* 'symbol 'expected (second arg)) global)

    
    (symbol? (second arg)) ;(setq x 7) && (setq x (+ x 1))
       (let [res (evaluar (last arg) global local)]
          (list 
              (first res)
              (actualizar-amb 
                 (second res) 
                 (second arg) 
                 (first res)
              )
          )
       )

  )
)

(defn evaluar-setq
  "Evalua una forma 'setq'. Devuelve una lista con el resultado y un ambiente actualizado."
  [arg global local]
  (if (<= (count arg) 3)

    (evaluar-setq-simple arg global local)


    (evaluar-setq 
       (concat '(setq) (drop 3 arg)) 
       (second (evaluar-setq-simple (take 3 arg) global local))
       local
    )
  )
)
