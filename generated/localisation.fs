module Localisation
// Generated code. Do not edit.

let locMap = Map [
    ("en-us", Localisation_en_us.Strings());
    ("ru-ru", Localisation_ru_ru.Strings());
]

let loc = Map.find "en-us" locMap
