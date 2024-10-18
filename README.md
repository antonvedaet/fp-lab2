# Лабораторная 2
---

**Студент:** Ведерников Антон Владимирович  
**ИСУ:** 367970  
**Группа:** P3323  
**Университет:** НИУ ИТМО  
**Факультет:** Программная инженерия и компьютерная техника  
**Курс:** 3-й курс  

---
# Отчет

## Цель

Цель данной лабораторной работы — освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing). В рамках работы была реализована структура данных — Open Addressing Set.

## Требования к разработанному ПО

1. Реализовать функции:
   - добавление и удаление элементов;
   - фильтрация;
   - отображение (map);
   - свертки (левая и правая);
   - структура должна быть моноидом.

2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based testing (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Использовать идиоматичный для технологии стиль программирования.

## Ключевые элементы реализации

### Определение структуры данных

Создание пустого множества

```haskell
empty :: Int -> OASet a
empty max_size = OASet (replicate max_size Nothing)
```

### Добавление элемента

Функция `insert` добавляет элемент в множество.

```haskell
insert :: (Hashable a) => a -> OASet a -> OASet a
insert x (OASet table) = go (hashIndex x (length table)) 0
  where
    max_size = length table
    go idx n
        | n >= max_size = OASet table
        | isNothing (table !! idx) = OASet (updateTable idx x table)
        | table !! idx == Just x = OASet table
        | otherwise = go ((idx + 1) `mod` max_size) (n + 1)

    updateTable idx val tbl = take idx tbl ++ [Just val] ++ drop (idx + 1) tbl
```

### Удаление элемента

Функция `remove` удаляет один элемент из множества.

```haskell
remove :: (Hashable a) => a -> OASet a -> OASet a
remove x (OASet table) = go (hashIndex x (length table)) 0
  where
    max_size = length table
    go idx n
        | n >= max_size = OASet table
        | table !! idx == Just x = OASet (take idx table ++ [Nothing] ++ drop (idx + 1) table)
        | otherwise = go ((idx + 1) `mod` max_size) (n + 1)
```

### Фильтрация

Функция `filter` фильтрует элементы в множестве по заданному предикату и возвращает новое множество:

```haskell
filter :: (a -> Bool) -> OASet a -> OASet a
filter predicate (OASet table) = OASet (go table)
  where
    go [] = []
    go (Nothing:xs) = Nothing : go xs
    go (Just x:xs)
      | predicate x = Just x : go xs
      | otherwise = Nothing : go xs
```

### Отображение (map)

Функция `map` применяет функцию к элементам множества и возвращает новое множество:

```haskell
map :: (a -> b) -> OASet a -> OASet b
map f (OASet table) = OASet (go table)
  where
    go [] = []
    go (Nothing:xs) = Nothing : go xs
    go (Just x:xs) = Just (f x) : go xs
```

### Свертки

Левая и правая свертки реализованы в функциях `foldl` и `foldr` соответственно:

```haskell
foldl :: (b -> a -> b) -> b -> OASet a -> b
foldl f acc (OASet table) = foldlTable acc table
  where
    foldlTable acc [] = acc
    foldlTable acc (Nothing:xs) = foldlTable acc xs
    foldlTable acc (Just x:xs) = foldlTable (f acc x) xs

foldr :: (a -> b -> b) -> b -> OASet a -> b
foldr f acc (OASet table) = foldrTable table acc
  where
    foldrTable [] acc = acc
    foldrTable (Nothing:xs) acc = foldrTable xs acc
    foldrTable (Just x:xs) acc = f x (foldrTable xs acc)
```

## Тестирование

### Unit Testing

[тесты](./test/Spec.hs)

### Property-Based Testing

[тесты](./test/PropertySpec.hs)

## Выводы

В ходе реализации лабораторной работы был изучен процесс создания неизменяемых структур данных на языке Haskell.
