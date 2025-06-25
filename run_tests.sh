#!/bin/bash

# Define paths relative to your home directory
EMACS_DOTD="$HOME/.emacs.d"
INIT_ORG="$EMACS_DOTD/Emacs.org"
INIT_EL="$EMACS_DOTD/init.el"
TEST_FILE="$EMACS_DOTD/tests/my-config-tests.el"

echo "--- Emacs Configuration Test Runner ---"

# 1. Ensure init.el is tangled from Emacs.org
if [ -f "$INIT_ORG" ]; then
    echo "Tangling Emacs.org to init.el..."
    # Launch Emacs with your full init.el and then explicitly call org-babel-tangle-file
    # This ensures Org-mode and all its dependencies (like org-babel) are properly loaded.
    emacs -Q --batch \
          -L "$EMACS_DOTD" \
          --load "$INIT_EL" \
          --eval "(progn
                     (setq org-confirm-babel-evaluate nil) ; Ensure no prompts
                     (org-babel-tangle-file \"$INIT_ORG\" \"$INIT_EL\"))" > /dev/null 2>&1

    TANGLE_RESULT=$?
    if [ $TANGLE_RESULT -ne 0 ]; then
        echo "Error tangling Emacs.org (exit code: $TANGLE_RESULT). Aborting tests."
        exit 1
    else
        echo "Tangling complete."
    fi
else
    echo "Warning: Emacs.org not found at $INIT_ORG. Proceeding with existing init.el."
fi

# 2. Run ERT tests in a clean Emacs batch session
echo "Running Emacs configuration tests using ERT..."

emacs -Q --batch \
      -L "$EMACS_DOTD" \
      --load "$INIT_EL" \
      --load "$TEST_FILE" \
      -f ert-run-tests-batch-and-exit

TEST_RESULT=$?

if [ $TEST_RESULT -eq 0 ]; then
    echo "All Emacs configuration tests passed successfully!"
else
    echo "Emacs configuration tests FAILED! Check the output above for details."
fi

exit $TEST_RESULT
