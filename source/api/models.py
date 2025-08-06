from django.db import models

from django.db import models

class UserSubmission(models.Model):
    text = models.CharField(max_length=255)      # Player name or text input
    counter = models.IntegerField(default=0)     # Score or count
    word = models.CharField(max_length=255, blank=True, null=True)  # Current word
    won = models.BooleanField(default=False)     # Whether player won
    submitted_at = models.DateTimeField(auto_now_add=True)  # Timestamp

    def __str__(self):
        return f"{self.text} - Score: {self.counter}"
