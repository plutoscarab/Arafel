module Localisation_ru_ru
// Generated code. Do not edit.

type Strings() =
    inherit Localisation_en_us.Strings()

    override _.IsNotBound(id) =
        $"«{id}» не связан, а не является значением."
