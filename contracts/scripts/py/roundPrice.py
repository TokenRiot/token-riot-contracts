from typing import Optional, Union

def get_price() -> int:

    quantity = 100*pow(10,6)
    factor = 57

    price_after_royalties = str(int(quantity / factor))
    print(price_after_royalties)


get_price()

