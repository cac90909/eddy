from core.infrastructure.cache.base_cache import BaseCache
from core.infrastructure.cache.cache_resource import CacheResource

class BaseCacheResourceManager:

    def __init__(self, newprop: str):
        self.yes = 123
        setattr(self, newprop, 12)


class ddd:

    def __init__(self, bcrm: BaseCacheResourceManager):
        self.bcrm = bcrm



class abc:

    abc123 = BaseCacheResourceManager("maybn")
    myddd = ddd(abc123)
    myddd.bcrm.
