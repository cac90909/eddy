from django.db import models
from django.contrib.auth import get_user_model

User = get_user_model()

class Snapshot(models.Model):
    snapshot_id = models.AutoField(primary_key=True)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name="snapshots")
    title = models.CharField(max_length=255)
    description = models.CharField(max_length=255)
    operation_chain = models.JSONField(default=list, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        db_table = 'snapshots'