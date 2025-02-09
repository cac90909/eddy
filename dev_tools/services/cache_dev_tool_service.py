from shared.services.cache_service import CacheService
from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.util import log_vars_vals_cls, catch_exceptions_cls

@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class CacheDevToolService:

    def __init__(self, **kwargs):
        self.cache_service = CacheService()
        self.explorer_cache_service = ExplorerCacheService()

    def handle_cache_operation(self, user_id, cache_type, operation_type, operation_params):
            if cache_type == "explorer":
                if operation_type == "get_user_cache":
                    user_cache = self.explorer_cache_service.get_user_cache(user_id=user_id)
                    return user_cache
                if operation_type == "get_user_cache_stats":
                    user_cache = self.explorer_cache_service.get_user_cache(user_id=user_id)
                    stats = {}
                    for obj, val in user_cache.items():
                        stats[obj] = len(val)
                    return stats
            


