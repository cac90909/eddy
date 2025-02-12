from shared.services.cache_service import CacheService
from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value={"success": False, "error": "Unhandled exception"})
class CacheDevToolService:
    def __init__(self, **kwargs):
        self.cache_service = CacheService()
        self.explorer_cache_service = ExplorerCacheService()

    def handle_cache_operation(self, user_id, cache_type, operation_type, operation_params):
        debug_print()
        if cache_type == "explorer":
            if operation_type == "get_cache_datasets":
                user_cache = self.explorer_cache_service.get_dataset_list(user_id=user_id)
                return user_cache
            elif operation_type == "get_user_cache_stats":
                # Compute summary statistics from the user cache.
                user_cache = self.explorer_cache_service.get_user_cache(user_id=user_id)
                stats = {}
                # For each subcache (like datasets, operation_chain), count the number of items.
                for key, value in user_cache.items():
                    # Assuming value is a list or QuerySet; if QuerySet, you may convert to a list first.
                    if hasattr(value, '__len__'):
                        stats[key] = len(value)
                    else:
                        stats[key] = 0
                return stats
            else:
                return {"success": False, "error": f"Operation type '{operation_type}' not supported for cache type '{cache_type}'."}
        else:
            return {"success": False, "error": f"Cache type '{cache_type}' not supported."}
