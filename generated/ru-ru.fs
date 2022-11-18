module Localisation_ru_ru

type Strings() =
    inherit Localisation_en_us.Strings()

    // ID 2
    override _.IsNotBound(id) =
        $"«{id}» не связан, а не является значением."
