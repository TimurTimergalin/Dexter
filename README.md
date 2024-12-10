# Dexter

**Dexter** - функциональный, ленивый, динамически типизированный язык программирования.

## Установка

Для начала нужно скомпилировать код интерпретатора. Если у вас *Windows*, все уже собрано в `Dexter.zip`. Для других платформ нужно скомпилировать проект `Cli` при помощи команды `dotnet publish -r *целевая платформа*`.

Затем, нужно сделать следующее:

1. Положить исполняемый файл в любую удобную папку;
2. Если вы компилировали программу самостоятельно, в ту же папку скопируйте папку `Std`;
3. Добавьте выбранную папку в PATH;

Использование: `.\Cli file.dxt` (или `./Cli file.dxt` - в зависимости от ОС), где `file.dxt`  - имя файла с исходным кодом.

Интерпретатор Dexter поддерживает сторонние библиотеки. Для их использования в папке с интерпретатором добавьте папку `Lib`, в которой и нужно хранить код сторонних библиотек.

## Возможности языка

### Создание констант

```
let a = "Константа"
let b = 3
let c = [1, 2, 3]
let d = 3.5
let e = True
let f = None
```

### Создание функций

```
let f = fun x y -> x y
let g x y = x - y
```

### Составные конструкции

```
let a = {
	let x = 2; let y = 4
	let z = 5
	x + y + z
}
```

### Определение типов

```
type TypeName: Cons1(), Cons2(arg1, arg2) {  # Названия аргументов нигде не используются - важно лишь их количество
	# Здесь можно определять функции - они будут помещены в пространство имен TypeName
	# У типов есть функции-члены inst (проверка на принадлежность типу), repr - преобразование в строку, и некоторые базовые операторы.
	# Всех их (кроме inst) можно переопределить
	# Здесь также можно определить любые (!) инфиксные операторы,
	# а также унарные операторы +, -, !, ~
	let (+) x y = {...} # Бинарный +
	let (^+) x = {...} # Унарный +
}
```



### Сопоставление шаблонов

```
let a = True
let b = match a {
	case True() -> 1; case False() -> 0;  # Скобки обязательны, иначе будет присовение переменной
}

let c = [1, 2, 3]
let d = match c {
	case [] -> None
	case [x, y, z] when !(x % 2) -> z
    case h::t -> t
}

let e = Node(1, Node(-5, End)) # эквивалентно [1, -5]
let f = match e {
	case Node(a, b) -> a
}

let g (h::[])
```



### Принудительное вычисление выражений

```
let res = print "123"
eval res  # Запускается всегда при интерпритации файла
let res1 = print "456"
entrypoint = res1  # Запустится только, если этот файл был запущен, но не при импорте
```

### Импорты

```
import "my_file.dxt" as myFile # Локальный импорт пользовательского файла
import * from "folder/another_file.dxt" as another # Добавляет все знаения из файла в глобальный скоуп. ПРи пересечении имен - новые имена игнорируются.
import "IO.dxt" as IO # Также возможны импорты из стандартной бибилиотеки и сторонних библиотек
import "C:/global.dxt" # Возможно, но не желательно

let a = myFile.member
let b = another.member
let c = member # из another
```

### Монаидические выражения

```
import "IO.dxt" as IO

entrypoint = exec {
	do inp <- IO.minput
	let res = inp + "1"
	do IO.mprintln res
}
```

### Другие примеры

Другие примеры можно найти в папке `Examples` и в стандартной библиотеке

