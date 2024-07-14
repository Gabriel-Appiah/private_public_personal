from git_filter_repo import FilterRepo


class RemoveSecret(FilterRepo):
    def process_blob(self, blob):
        if blob.path.decode('utf-8') == 'Data/FinalprocesEdited.csv':
            lines = blob.data.decode('utf-8').split('\n')
            if len(lines) > 1591:
                lines[1591] = ''  # Remove the secret at line 1592 (0-indexed)
                blob.data = '\n'.join(lines).encode('utf-8')


RemoveSecret().run()
