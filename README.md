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

let a = myFile.member
let b = another.member
let c = member # из another
```







