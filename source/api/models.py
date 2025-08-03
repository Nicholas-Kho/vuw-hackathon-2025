from django.db import models

class UserSubmission(models.Model):
    text = models.TextField()
    submitted_at = models.DateTimeField(auto_now_add=True)

    def __str__(self):
        return self.text[:50]  # show first 50 chars
